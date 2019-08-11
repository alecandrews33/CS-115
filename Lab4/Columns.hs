module Main where


import Prelude
import System.Environment
import System.Exit
import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do
  args <- getArgs
  -- Check that there is at least one column input that is a positive
  -- integer
  if (length args) < 2 || (length (filter (\x -> x /= "0") 
    (filter validInt args)) /= (length args) - 1)
    then do 
      name <- getProgName
      die ("Usage " ++ name ++ " n1 n2 ... filename \
      \(n1, n2 ... must be positive integers)")
  else 
    let cols = map (\x -> read x :: Int) (take (length args - 1) args) in 
      (if (last args) == "-"
        then do 
          file <- hGetContents stdin
          let file_lines = lines file
          let file_cols = map (columnExtractor cols) file_lines
          putStrLn (intercalate "\n" file_cols)    
      else 
        do 
          file <- readFile (last args)
          let file_lines = lines file 
          let file_cols = map (columnExtractor cols) file_lines
          putStrLn (intercalate "\n" file_cols))
          
          
          
          
-- helper function to tell if a number is a positive int
validInt :: String -> Bool
validInt [] = False
validInt [x] = isDigit x
validInt (x:xs) = validInt [x] && validInt xs

-- helper function to extract proper columns
columnExtractor :: [Int] -> String -> String
columnExtractor cols str = 
  let str_words = words str 
      valid_columns = filter (\x -> x <= length str_words) cols
      column_words = map (\x -> str_words !! (x - 1)) valid_columns in 
    intercalate " " column_words
    
    
   
    
