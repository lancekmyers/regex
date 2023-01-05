module Main where

import Regex 
import Parser 
import qualified System.Environment as System
import qualified Data.ByteString.Char8 as BS
import System.IO.Posix.MMap ( unsafeMMapFile ) 
import System.IO (hFlush, stdout)


debugger :: String -> IO () 
debugger reStr = print re >> interaction 
  where 
    re = parseRE reStr
    interaction = do 
      putStr "»» " >> hFlush stdout
      line <- getLine
      case line of 
        ":q" -> putStrLn "Bye!"
        ':':'r':'e':' ':rest -> debugger rest
        _ -> if matches (BS.pack line) re 
          then putStrLn "Match" >> interaction
          else putStrLn "Fail"  >> interaction


finder :: String -> String -> IO ()
finder reStr fname = do 
  let re = parseRE reStr 
  
  fileLines <- BS.lines <$> unsafeMMapFile fname
  let found = filter (\l -> matches l re) fileLines 
  
  mapM BS.putStrLn found
  pure ()


main :: IO ()
main = do
  args <- System.getArgs 

  case args of {
    ["debug", givenRE] -> debugger givenRE;
    [givenRE, fname] -> finder givenRE fname; 
    _ -> error "Bad arguments\nExpects <regular-exp> <file-name>"
  }
