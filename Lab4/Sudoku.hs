--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO
import Prelude

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = 
  iter s (1, 1)
  where
    -- Find the next value in the array
    nextValue :: (Int, Int) -> (Int, Int) 
    nextValue (x, y) | x == 9 = (1, y + 1)
                     | otherwise = (x + 1, y)
                     
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter s (i, j) = do 
      vals <- getOKValues s (i, j)
      current <- readArray s (i, j)
      if current == 0 
      then iter' s (i, j) vals
      else if i == 9 && j == 9 then return (True)
        else  
          let next = nextValue (i, j) in do
            iter s next
   


    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' s (i, j) (v:vs) = do
      writeArray s (i, j) v
      let next = nextValue (i, j) in 
        if i == 9 && j == 9 then return (True)
          else do
            valid <- iter s next
            if valid then return (True)
              else do
                writeArray s (i, j) 0
                iter' s (i, j) vs
    iter' _ (_, _) [] = return False
      
      
      

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues s (i, j) = do
      row <- getRow s i
      col <- getCol s j
      box <- getBox s (i, j)
      return ([1..9] \\ (union (union row col) box))

    -- Return the ith row in a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow s i = do 
      mapM (\x -> readArray s (i, x)) [1 .. 9] >>= return

    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol s i = do
      mapM (\x -> readArray s (x, i)) [1 .. 9] >>= return
      
    -- Get elements in box. Input is center of box.
    boxReader :: Sudoku -> (Int, Int) -> IO [Int]
    boxReader s (i, j) = do 
      n1 <- readArray s (i - 1, j - 1)
      n2 <- readArray s (i, j - 1)
      n3 <- readArray s (i + 1, j - 1)
      n4 <- readArray s (i - 1, j)
      n5 <- readArray s (i, j)
      n6 <- readArray s (i + 1, j)
      n7 <- readArray s (i - 1, j + 1)
      n8 <- readArray s (i, j + 1)
      n9 <- readArray s (i + 1, j + 1)
      return (n1:n2:n3:n4:n5:n6:n7:n8:n9:[])

    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox s (i, j) = do      
      let x = ((i - 1) `div` 3) * 3 + 2
      let y = ((j - 1) `div` 3) * 3 + 2
      
      boxReader s (x, y)

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

