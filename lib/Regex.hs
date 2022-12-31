{-# LANGUAGE PatternSynonyms, BangPatterns #-}
{-# LANGUAGE ViewPatterns, MonadComprehensions #-}

module Regex (pattern Sym, pattern Null, pattern NotNull, Regex, 
    cat, or_, and_, concatAnd, concatCat, concatOr, 
    star, not_, matches, 
    classFromList, fromString
    ) where

import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Internal as Seq
import Data.Sequence (Seq)
import Data.Foldable (Foldable(fold, foldl', foldr, foldr'))
import Control.Applicative (Applicative(liftA2))
import Set (Set, union, intersect, member, compl)
import qualified Set as Set 
import qualified Data.ByteString.Lazy.Char8 as BS 

-- | Regex for single character/symbol 
pattern Sym :: Char -> Regex 
pattern Sym a = CS (Set.Sing a)

-- | Regex for empty set
-- note that this is not the empty *string*
pattern Null :: Regex
pattern Null = CS Set.Null

-- | Regex matching not null, i.e. everything 
-- should maybe renamed Any/Dot or something
pattern NotNull :: Regex 
pattern NotNull = CS Set.NotNull

data Regex  
    = CS Set
    | Cat (Seq Regex)
    | And (Seq Regex)
    | Or  (Seq Regex)
    | Not Regex
    | Star Regex
    -- | Capture Regex -- Is this a good idea??
    | Str BS.ByteString 
    | Empty 
    deriving (Show, Eq, Ord)

-- | Checks if regex are equal-ish 
-- This is not actually equality, might sub out for a different approximate 
-- equality check at some point.
-- Note that Eq is derived and necessary for Ord instance, should either 
-- not derive those instances, or implement a better equality check for Eq.
eqIsh :: Regex -> Regex -> Bool 
eqIsh (CS   a) (CS   b) = a == b 
eqIsh (Cat  a) (Cat  b) = a == b
eqIsh (Or   a) (Or   b) = a == b
eqIsh (And  a) (And  b) = a == b
eqIsh (Not  a) (Not  b) = a == b 
eqIsh (Star a) (Star b) = a == b 
eqIsh Empty Empty = True  
eqIsh _ _ = False


{-# INLINE doubleton #-} 
-- | Construct a sequence of length two 
-- This is just the inlined and specialized def from Data.Seq.Internal
doubleton :: Regex -> Regex -> Seq.Seq Regex 
doubleton r s = {-# SCC seq_pair_construction #-} Seq.Seq $ 
    Seq.Deep 2 (Seq.One (Seq.Elem r)) Seq.EmptyT (Seq.One (Seq.Elem s))


{-# INLINE cat #-}
-- | Concatenate regexes together, simplifying when easy and possible
cat :: Regex -> Regex -> Regex
cat Empty r = r
cat r Empty = r 
cat Null _ = Null 
cat _ Null = Null 
cat (Star NotNull) (Star NotNull) = (Star NotNull)
cat (Str a) (Str b) = Str (a <> b)
cat (Sym a) (Str b) = Str (BS.cons a b)
cat (Str a) (Sym b) = Str (BS.snoc a b)
cat (Cat r) (Cat t) = {-# SCC seq_concat #-} Cat $ r Seq.>< t
cat (Cat r) t = {-# SCC seq_snoc #-} Cat $ r Seq.|> t 
cat r (Cat t) = {-# SCC seq_cons #-} Cat $ r Seq.<| t
cat r s = Cat (doubleton r s)

-- | Concatenate several regexes together.
-- Note that sequencing regexes is *order dependent*, so should probably not 
-- be used with foldable containers that do not guarantee the order of their elements
concatCat :: (Foldable f) => f Regex -> Regex
concatCat = foldl cat Empty

-- | `Or` several regexes together.
concatOr :: (Foldable f) => f Regex -> Regex
concatOr = foldl or_ Null

-- | `And` several regexes together.
concatAnd :: Foldable f => f Regex -> Regex 
concatAnd = foldl and_ NotNull

{-# INLINE concatCat #-}
{-# INLINE concatAnd #-}
{-# INLINE concatOr  #-}


{-# INLINE or_ #-}
-- | `Or` two regexes together, making simplifications when possible and easy 
or_ :: Regex -> Regex -> Regex 
or_ Null x = x 
or_ x Null = x 
or_ _ NotNull = NotNull
or_ NotNull _ = NotNull
or_ (Star NotNull) _ = (Star NotNull)
or_ _ (Star NotNull) = (Star NotNull)
or_ (CS a) (CS b) = CS $ a `union` b
or_ (Or x) (Or y) = {-# SCC seq_concat #-} Or $ x Seq.>< y 
or_ (Or r) s = {-# SCC seq_snoc #-} Or $ r Seq.|> s
or_ r (Or s) = {-# SCC seq_cons #-} Or $ r Seq.<| s
or_ r s 
    | (r `eqIsh` s)    = r
    | otherwise = Or (doubleton r s)


{-# INLINE and_ #-}
-- | `And` two regexes together, making simplifications when possible and easy 
and_ :: Regex -> Regex -> Regex
and_ Null _ = Null 
and_ _ Null = Null 
and_ NotNull r = r
and_ r NotNull = r
and_ (And r) (And s) = {-# SCC seq_concat #-} And $ r Seq.>< s
and_ (And r) s = {-# SCC seq_snoc #-} And $ r Seq.|> s 
and_ r (And s) = {-# SCC seq_cons #-} And $ r Seq.<| s
and_ r s 
    | (r `eqIsh` s) = r 
    | otherwise = And (doubleton r s)
{-# SCC and_ #-}


{-# INLINE star #-}
-- | Kleene star, making simplifications when possible and easy 
star :: Regex -> Regex
star (Star r) = star r 
star Empty = Empty 
star Null  = Empty 
star (Not (Not r)) = r 
star r = Star r

{-# INLINE not_ #-}
-- | Negation of regex, making simplifications when possible and easy 
not_ :: Regex -> Regex
not_ (Not r) = r 
not_ (CS s) = CS $ compl s
not_ r = Not r 

{-# INLINE nullable #-}
-- | Is the given regex nullable? 
-- In other words, does it match the empty string?
nullable :: Regex -> Bool 
nullable Empty = True  
nullable Null = False
nullable (CS   _) = False 
nullable (Cat rs) = all nullable rs
nullable (And rs) = all nullable rs
nullable (Or  rs) = any nullable rs
nullable (Star r) = True  
nullable (Not  r) = not $ nullable r 
nullable (Str  x) = BS.null x

-- | Brzozowski derivative 
-- Given a string `str@(a:as)` and regex `re` 
-- `deriv a re` is the regex that `as` must match in order for 
-- `str` to match `re`
deriv :: Char -> Regex -> Regex 
deriv a (CS s) 
    | member a s = Empty 
    | otherwise = Null
deriv _ Null  = Null 
deriv _ Empty = Null 
deriv a (Cat (r Seq.:<| s)) 
    | nullable r = {-# SCC deriv_cat #-} case s of 
        -- ((deriv a r) `cat` Cat s) `or_` (deriv a (Cat s))
        s' Seq.:<| Seq.Empty -> ((deriv a r) `cat` s') `or_` (deriv a s')
        Seq.Empty         -> deriv a r
        _ -> ((deriv a r) `cat` (Cat s)) `or_` (deriv a (Cat s))

    | otherwise  = {-# SCC deriv_cat #-} case s of   
        s' Seq.:<| Seq.Empty -> (deriv a r) `cat` s'
        Seq.Empty            -> deriv a r
        _                    -> (deriv a r) `cat` Cat s
deriv a (Cat Seq.Empty) = Null  
-- deriv a (Cat r s) = or_ (cat (deriv a r) s) (cat (nu r) (deriv a s))
deriv a (Star r) = cat (deriv a r) (Star r)
deriv a (Or rs)  = concatOr $ deriv a <$> rs 
deriv a (And rs) = concatAnd $ deriv a <$> rs 
deriv a (Not r) = not_ (deriv a r)
deriv a (Str x) = case BS.uncons x of 
    Just (x, xs) 
        | a == x -> Str xs 
        | otherwise -> Null 
    Nothing -> Null

-- | Reference implementation of matching with derivatives 
matches' :: [Char] -> Regex -> Bool 
matches' _ Null = False 
matches' [] re  = nullable re
matches' (c : cs) re = matches' cs (deriv c re)


-- | Checks if given string maches the regex 
matches :: BS.ByteString -> Regex -> Bool 
matches txt Null = False 
matches _ (Star NotNull) = True
matches txt (Str txt') = txt == txt'
matches txt (Cat (Str txt' Seq.:<| rest)) = case BS.stripPrefix txt' txt of 
    Just rem -> matches rem (Cat rest)
    Nothing  -> False
matches txt pat@(Cat ((Star NotNull) Seq.:<| rest@(Str txt' Seq.:<| _)))
    | BS.length txt >= BS.length txt' = case BS.elemIndex (BS.head txt') txt of 
        Just jmp -> matches (BS.drop jmp txt) (Cat rest `or_` pat)
        Nothing -> False  
    | otherwise = False
matches txt pat@(Cat ((Star NotNull) Seq.:<| rest@(Sym c Seq.:<| _))) = 
    case BS.elemIndex c txt of 
        Just jmp -> matches (BS.drop jmp txt) (Cat rest `or_` pat)
        Nothing -> False  
matches txt re = case BS.uncons txt of 
    Just (c,cs) -> matches cs $ deriv c re 
    Nothing     -> nullable re

-- | Build a set of characters from a list 
classFromList [] = Null 
classFromList [x] = Sym x
classFromList x = CS (Set.fromList x)

-- | Build a regex that matches the given string 
fromString = Str . BS.pack 


-- handful of single character regexes for 
-- constructing tests and demos 
a = Sym 'a'
b = Sym 'b'
c = Sym 'c'
d = Sym 'd'