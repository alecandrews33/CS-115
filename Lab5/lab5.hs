import Control.Monad
import Prelude

-- A.1
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do
  i <- [1..]
  j <- [1..i-1]
  k <- [1..j-1]
  l <- [1..k-1]
  guard $ i^3 + l^3 == j^3 + k^3
  return ((i,l), (j, k), i^3 + l^3)

-- A.2
natural_sum :: Integer
natural_sum = sum $ do 
  i <- [1..999]
  guard $ i `mod` 3 == 0 || i `mod` 5 == 0
  return i
  
natural_sum2 :: Integer
natural_sum2 = sum $ do 
  i <- [1..999]
  if i `mod` 3 == 0 || i `mod` 5 == 0 then return i else mzero

-- 233168

-- A.3
isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)

largestPalindrome :: Integer
largestPalindrome = maximum $ do 
  i <- [100..999]
  j <- [100..999]
  guard $ isPalindrome (i * j)
  return (i * j)
  
-- 906609
  
-- A.4
type Expr = [Item]

data Item = N Int | O Op
  deriving Show
  
data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

exprs :: [Expr]
exprs = do 
  op1 <- ops
  op2 <- ops
  op3 <- ops
  op4 <- ops
  op5 <- ops
  op6 <- ops
  op7 <- ops
  op8 <- ops
  return [N 1, op1, N 2, op2, N 3, op3, N 4, op4, N 5, op5, N 6, op6,
    N 7, op7, N 8, op8, N 9]

cat :: Int -> Int -> Int
cat n1 n2 = (n1 * 10) + n2 

normalize :: Expr -> Expr
normalize [] = []
normalize (N n1 : []) = [N n1]
normalize ((N n1) : (O Cat) : (N n2) : rest) = normalize (N (cat n1 n2)
 : rest)
normalize ((N n1) : (O op) : rest) = (N n1) : (O op) : normalize rest
 
normalize _ = error ("Expression not valid")

evaluate :: Expr -> Int
evaluate [] = 0
evaluate (N n1 : []) = n1
evaluate ((N n1) : (O Add) : (N n2) : rest) = evaluate (N (n1 + n2) : 
  rest)
evaluate ((N n1) : (O Sub) : (N n2) : rest) = evaluate (N (n1 - n2) : 
  rest)
evaluate _ = error ("Expression not valid")

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs

-- B.1

{-
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f lst = concat (map f lst)

(>>=) :: [a] -> (a -> [b]) -> [b]
mv >>= f = concatMap f mv


[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)

[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >>= \_ -> return (n1, n2)

[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> return (n1, n2) [])

[1..6] >>= \n1 -> [1..6] >>= \n2 -> []

[1..6] >>= \n1 -> concatMap (\n2 -> []) [1..6]

[1..6] >>= \n1 -> []

concatMap (\n1 -> []) [1..6]

[]
-}

-- B.2

{-
do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)
   
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> 
  return (n1, n2)
  
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >>= 
  \_ -> return (n1, n2)
  
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> return (n1, n2)) 
  (return <anything>)
  
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concat (map (\_ -> return (n1, n2)) 
  (return <anything>)
  
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concat (return (n1, n2))

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)

do n1 <- [1..6]
   n2 <- [1..6]
   return (n1, n2)
   
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)

These are reduced to the same thing!
-}

-- B.3

{-

let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
     return [c1, c2]

s >>= 
  \y = case y of 
    ('a', 'a', c1, c2, 'b', 'b') -> return [c1, c2]
    _ -> fail s
    
replace s to get:

["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>=
  \y = case y of 
    ('a', 'a', c1, c2, 'b', 'b') -> return [c1, c2]
    _ -> fail s
    
Replace >>=

concatMap (\y = 
  case y of ('a', 'a', c1, c2, 'b', 'b') -> return [c1, c2]
  _ -> fail s) ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
  
concat (map (\y = 
  case y of ('a', 'a', c1, c2, 'b', 'b') -> return [c1, c2]
  _ -> fail s) ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
  
concat (return [x, y], return [z, w], [], return [c, c], [])

["xy", "zw", "cc"]

The error function could not be used because it would terminate the 
process when it was given something that did not match the pattern.
Using this form of fail allows for a graceful fail so that pattern 
matching is possible. 

-}

-- B.4

{- 

Given m = [x1, x2, ...]
k = foldr ((++) . k) [] [x1, x2, ...]
k = foldr (\y -> (++) (k y)) [] [x1, x2, ...]
k = [k x1, k x2, ...]

k = concat (map k [x1, x2, ...])
k = concat ([k x1, k x2, ...])
k = [k x1, k x2, ...]

Given m = []
k = foldr ((++) . k) [] []
k = foldr (\y -> (++) (k y)) [] []
k = []

k = concat (map k [])
k = concat ([])
k = []

-}

-- B.5

{-

The error is thrown when the code gets to AnyNum (n + s). n and s are 
both instances of the Num type class, but they are not necessarily the 
same type. Thus, the addition operator cannot function with two 
different types because it does not know which form to assume. The 
AnyNum datatype needs to be changed to fix this issue with anySum, 
because not enough constraints are imposed on the elements currently.

-}


    
    

