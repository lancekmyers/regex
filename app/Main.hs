module Main where

import Regex 
import qualified System.Environment as System
import qualified Data.ByteString.Lazy.Char8 as BS

-- | build a regex that matches words given wordle state 
-- >>> wordle "_a___" "<yellow lettters here>" "grey letters here"
wordle :: String -> String -> String -> Regex 
wordle green gray yellow = no_gray `and_` contains_yellow
  where 
    gr = not_ $ classFromList gray
    contains_yellow = concatAnd [ 
      (star NotNull) `cat` Sym c `cat` (star NotNull)
      | c <- yellow ]
    no_gray = concatCat [ case c of 
      '_' -> gr
      c   -> Sym c
      | c <- green ]

-- | regex that matches only the word havoc 
havoc = concatCat $ Sym <$> "havoc"

-- | regex that matches words with the right letters in the right spots, 
-- but does not check the yellow or gray letter conditions 
wordleGreen green = concatCat [ case c of 
      '_' -> NotNull
      c -> Sym c 
    | c <- green 
  ]

-- | regex that matches words on green and gray conditions 
wordleGreenGray green gray = no_gray
  where 
    grayLetters = classFromList gray
    no_gray = concatCat [ case c of 
      '_' -> not_ grayLetters
      c   -> Sym c
      | c <- green ]


main :: IO ()
main = do
  args <- System.getArgs 
  let re = case args of {
    [green] -> wordleGreen green;
    [green, gray] -> wordleGreenGray green gray  ;
    [green, gray, yellow] -> wordle green gray yellow ;
    _ -> error "Invalid arguments";
  }
  dict <- BS.lines <$> BS.readFile "/usr/share/dict/words"
  let solns = filter (\l -> matches l re) dict 
  BS.putStrLn $ BS.intercalate (BS.pack "\n") solns 
