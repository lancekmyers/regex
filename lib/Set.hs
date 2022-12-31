{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Set (
    Set, member, union, intersect, compl, 
    pattern Null, 
    pattern NotNull, 
    pattern Sing, 
    fromList
    ) where 

import Data.List (intercalate)
import qualified Data.IntSet as S
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable (Foldable(fold, foldl'))
import Control.Applicative (Applicative(liftA2))

data Set 
    = Set S.IntSet 
    | Any 
    | None 
    | Sing Char 
    | Complement Set
    | Union (Seq Set)
    | Intersect (Seq Set)
    deriving (Eq, Ord) 

member :: Char -> Set -> Bool
member _ Any = True 
member _ None = False 
member c (Sing c') = c == c'
member a (Set s) = S.member ( fromEnum  a) s
member a (Complement s) = not $ member a s 
member a (Union ss) = any (member a) ss
member a (Intersect ss) = all (member a) ss

instance Show Set where 
    show (Set as) = "{" ++ (intercalate ", " . fmap (show . toEnum @Char ) . S.toList $ as) ++ "}"
    show (Complement x) = "∑ \\ " ++ show x 
    show (Sing x) = "{" ++ show x ++ "}"
    show Any = "∑"
    show None = "{}"

-- this does *not* canonicalize!!! 
union :: Set -> Set -> Set 
union Any _ = Any 
union _ Any = Any 
union None x = x 
union x None = x
union (Sing x) (Set a) = Set $ S.insert (fromEnum x) a
union (Set a) (Sing x) = Set $ S.insert (fromEnum x) a
union (Sing a) (Sing b) = if a == b 
    then Sing a 
    else Set $ S.fromList [fromEnum a, fromEnum b]
union (Set a) (Set b) = Set (S.union a b) 
union (Union a) (Union b) = Union $ a Seq.>< b 
union a (Union b) = Union (a Seq.<| b) 
union (Union a) b = Union (a Seq.|> b) 
union a b = Union $ a Seq.<| Seq.singleton b 

intersect :: Set -> Set -> Set
intersect Any x = x 
intersect x Any = x
intersect x None = None 
intersect None x = None 
intersect x@(Sing c) y = if c `member` y then x else None
intersect x y@(Sing c) = if c `member` x then y else None
intersect (Set a) (Set b) = Set (S.intersection a b)
intersect (Union a) b = Union $ (flip intersect) b <$> a
intersect a (Union b) = Union $ intersect a <$> b
intersect (Intersect a) (Intersect b) = Intersect (a Seq.>< b) 


compl :: Set -> Set 
compl Any = None 
compl None = Any 
compl s@(Sing _) = Complement s
compl (Complement s) = s 
compl (Union s) = foldl intersect (Intersect Seq.empty) (compl <$> s)
compl (Intersect s) = foldl union (Union Seq.empty) (compl <$> s)
compl (Set s) = Complement (Set s)

fromList :: [Char] -> Set 
fromList [] = None
fromList [x] = Sing x 
fromList xs = Set . S.fromList . fmap fromEnum $ xs 

pattern Null :: Set 
pattern Null = None 

pattern NotNull :: Set 
pattern NotNull = Any 