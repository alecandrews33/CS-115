module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)
  
-- C.1 

sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix _ (r, c) | (r < 1) || (c < 1) = error("Bounds must be " ++
                                              "at least 1")
sparseMatrix [] b = SM b S.empty S.empty M.empty
sparseMatrix lst b@(r, c) 
  | all (\((i, j), _) -> i <= r && j <= c) lst == False = error("Out "
    ++ "of bounds error")
  | otherwise = SM b rows cols (M.fromList values)
    where 
      -- We only want nonzero values
      values = filter (\(_, b) -> b /= 0) lst
      rows = S.fromList (map (\((i, _), _) -> i) values)
      cols = S.fromList (map (\((_, j), _) -> j) values)
      
-- C.2 

addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM b1 _ _ v1) (SM b2 _ _ v2) 
  | b1 /= b2 = error("Bounds must be the same") 
  | otherwise = SM b1 rows cols v
    where
      -- We take the union so that all entries are added together
      v = M.filter (/= 0) (M.unionWith (+) v1 v2)
      rows = S.fromList (map (\(i, _) -> i) (M.keys v))
      cols = S.fromList (map (\(_, j) -> j) (M.keys v))
      
-- C.3

negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b rows cols v) = SM b rows cols (M.map (\n -> -n) v)

-- C.4

subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM s1 s2 = addSM s1 (negateSM s2)

-- C.5

mulHelper :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> 
  Integer -> Integer -> a
mulHelper (SM _ _ _ v1) (SM _ _ _ v2) i j = 
  let 
    -- Find the entries for the specified column and row
    row = M.filterWithKey (\(x, _) _ -> x == i) v1
    col = M.filterWithKey (\(_, y) _ -> y == j) v2
    -- Change how the key is represented to allow for intersection
    row_cols = M.mapKeys (\(_, c) -> c) row
    col_rows = M.mapKeys (\(r, _) -> r) col 
    -- We want to multiply together rows and columns of same value,
    -- so we use intersection, then take the elements and sum them
    -- since this will be the result for that entry in the resulting 
    -- matrix.
    intersect = M.elems (M.intersectionWith (*) row_cols col_rows) in
    sum (intersect)

mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM s1@(SM (r1, c1) rows _ _) s2@(SM (r2, c2) _ cols _) 
  | c1 /= r2 = error("Incompatible shapes!")
  | otherwise = 
    let 
      -- These are the rows and columns that aren't empty
      valid_rows = S.toList rows
      valid_cols = S.toList cols 
    in sparseMatrix 
      -- Use list comprehension to feed in nonempty rows and columns.
      -- Multiply all of these together.
      [((i, j), (mulHelper s1 s2 i j)) | i <- valid_rows, j <- valid_cols]
        (r1, c2)
      

-- C.6

getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (r, c) _ _ v) (i, j) 
  | i > r || j > c || i < 1 || j < 1 = error("Out of bounds")
  -- Default to a value of 0.
  | otherwise = M.findWithDefault 0 (i, j) v
  
rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM (r, _) _ _ _) = r

colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM (_, c) _ _ _) = c

-- C.7 

(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM

-- C.8

{- 
The Num type class typically represents numerical representations. 
A matrix is not a number, but rather a collection of numbers. It has 
different properties: rows, columns, determinant, trace that normal 
numbers do not have. It's not obvious how functions applied to numbers
would be translated for matrices. Thus, it's better off we don't 
force this data structure into this type class and allow it to have 
its own properties.
-}


