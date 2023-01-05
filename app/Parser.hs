module Parser where 

import Regex 
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

pRE :: ReadP Regex  
pRE = look >>= \x -> case x of 
    [] -> pfail -- return Empty 
    '(':_ -> (between (char '(') (char ')') pRE) >>= pPostfix
    '*':_ -> pfail 
    ')':_ -> pfail 
    '!':_ -> pfail 
    '|':_ -> pfail 
    '&':_ -> pfail 
   -- '.':_; -> (NotNull `cat`) <$> pRE 
    c:_ -> do 
        re <- get >>= \x -> case x of 
            '.' -> return  NotNull 
            '\\' -> Sym <$> get
            _ -> return $ Sym c
        pPostfix re    

pPostfix re = do 
    postfix <- option Nothing 
            (Just <$> (char '&' <|> char '|' <|> char '*' <|> char '!'))    
    case postfix of 
        Just '&' -> (and_ re) <$> pRE 
        Just '|' -> (or_ re) <$> pRE 
        Just '*' -> return (star re)
        Just '!' -> return (not_ re)
        Nothing  -> return re 

parseRE :: String -> Regex
parseRE str = fst . last $ readP_to_S (concatCat <$> many pRE) str
