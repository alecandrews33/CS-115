module Main where


import Prelude
import System.Environment
import System.Exit
  
main :: IO ()
main = do 
  args <- getArgs
  case (length args) of 
  1 -> do 
    file <- readFile (head args)
    line_list <- lines file
    reverse_line_list <- reverse line_list
    MapM_ putStrLn reverse_line_list
    exitSuccess
  _ -> do 
    name <- getProgName
    exitFailure ("Usage: " ++ name ++ " filename")

