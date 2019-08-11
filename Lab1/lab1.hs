-- B.1.1

{- Definition for op called +* that computes the sum of squares of 
its arguments. Assume both arguments are DoubleS. Make it 
left-associative and give it a precedence of 7. -}

(+*) :: Double -> Double -> Double
(+*) x y = x^2 + y^2
infixl 7 +*

-- B.1.2

{- Definition for an operator called ^|| that computes the exclusive
-OR of its two (boolean) arguments. Make it right-associative with a 
precedence of 3. Your definition should be only two (very simple) lines
(not counting the type declaration) and shouldn't use if. -}

(^||) :: Bool -> Bool -> Bool
(^||) False y = y
(^||) True y = (not y)
infixr 3 ^||

--B.2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x > y = error("The first value must be less than" ++
    "the second.")
                 | x == y = y
                 | otherwise = x * (rangeProduct (x + 1) y)
                               

-- B.3
prod :: [Integer] -> Integer
prod = foldr (*) 1

rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y | x > y = error("The first value must be less than" ++
    "the second.")
                  | otherwise = prod [x .. y]
                  

-- B.4.1
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] _ = []
map2 f _ [] = []
map2 f (x:xs) (y:ys) = (f x y) : (map2 f xs ys)

-- B.4.2
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 f [] _ _ = []
map3 f _ [] _ = []
map3 f _ _ [] = []
map3 f (x:xs) (y:ys) (z:zs) = (f x y z) : (map3 f xs ys zs)
        

-- B.4.3
{-
dot :: [Integer] -> [Integer] -> Integer
dot = (sum .) . map2 (*)

dot lst1 lst2
((sum .) . map2 (*)) lst1 lst2
((sum .) (map2 (*))) lst1 lst2
((\x -> sum . x) (map2 (*))) lst1 lst2
(sum (map2 (*))) lst1 lst2
sum (((map2 (*)) lst1) lst2)
sum ((map2 (*) lst1) lst2)
sum (map2 (*) lst1 lst2)
-}

-- B.5
answer = sum ([x | x <- [1 .. 999], mod x 3 == 0 || mod x 5 == 0])

-- 233168

-- B.6

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\y -> mod y x /= 0) xs)


primes = sieve [2 ..]
result = sum $ takeWhile (< 10000) primes

-- 5736396

--C.1
{- Instead of using lst and then using the head and tail functions 
to extract those parts of the list, we can write the argument better. -}
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

-- C.2 
{- This function was using the length function instead of pattern 
matching on an empty list and a singleton list.-}
largest :: [Integer] -> Integer
largest [] = error("empty list")
largest [x] = x
largest (x:xs) = max x (largest xs)
        
-- D.1
{-
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fib 3 
fib (3 - 1) + fib (3 - 2) 
fib 2 + fib (3 - 2)
fib (2 - 1) + fib (2 - 2) + fib (3 - 2) 
fib 1 + fib (2 - 2) + fib (3 - 2)
1 + fib (2 - 2) + fib (3 - 2)
1 + fib 0 + fib (3 - 2)
1 + 0 + fib (3 - 2)
1 + fib (3 - 2)
1 + fib 1
1 + 1
2
-}

--D.2
{- 
fact :: Integer -> Integer
fact n = n * fact (n - 1)
fact 0 = 1

fact 3
3 * fact(3 - 1)
3 * (3 - 1) * fact(3 - 1 - 1) 
3 * (3 - 1) * (3 - 1 - 1) * fact(3 - 1 - 1 - 1)
and so on...

The function is wrong becauuse it never terminates. The base case 
is put after the general case, so it is never reached. The 
function will just continue forever without ever touching its base case.
To fix it, just swap the order of fact n and fact 0. Since it won't
terminate, this will likely result in a stack overflow.
-}

-- D.3
{-
reverse :: [a] -> [a]
reverse xs = iter xs []
    where
        iter :: [a] -> [a] -> [a]
        iter [] ys = ys
        iter (x:xs) ys = iter xs (x:ys)
        
reverse [1, 2, 3]
iter [1, 2, 3] []
iter [2, 3] [1]
iter [3] [2, 1]
iter [] [3, 2, 1]
[3, 2, 1]

The asymptotic time complexity of this function as a function of the 
length of the input list is O(n). The reason for this is because 
the function iterates through the list and adds each element to the 
front of a new list. This requires one full sweep through the input 
list, so the complexity is O(n).
-}

--D.4 

{- 
reverse :: [a] -> [a] 
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

reverse [1, 2, 3]
(reverse [2, 3]) ++ [1]
(reverse [3] ++ [2]) ++ [1]
((reverse [] ++ [3]) ++ [2]) ++ [1]
(([] ++ [3]) ++ [2]) ++ [1]
(([3] ++ [2]) ++ [1])
3 : (([] ++ [2]) ++ [1])
3 : ([2] ++ [1])
3 : (2 : ([] ++ [1]))
3 : (2 : [1])
3 : 2 : [1]
[3, 2, 1]

{- This is O(n^2). Concatenations take O(n) time and we must iterate
through the entire list, which is O(n). -}



-- D.5
{-
isort :: [Integer] -> [Integer]
isort [] = []
isort (n:ns) = insert n (isort ns)
    where
        insert :: Integer -> [Integer] -> [Integer]
        insert n [] = [n]
        insert n m@(m1:_) | n < m1 = n : m
        insert n (m1:ms) = m1 : insert n ms
        
head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

head (isort [3, 1, 2, 5, 4])
head (insert 3 (isort [1, 2, 5, 4]))
head (insert 3 (insert 1 (isort [2, 5, 4])))
head (insert 3 (insert 1 (insert 2 (isort [5, 4]))))
head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
head (insert 3 (insert 1 (2 : (4 : insert 5 []))))
head (insert 3 (1 : (2 : (4 : insert 5 []))))
head (1 : insert 3 (2 : (4 : insert 5 [])))
1

1
-}

-- D.6
{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _init [] = init
foldr f init (x:xs) = f x (foldr f init xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x:xs) = foldl f (f init x) xs

foldr max 0 [1, 5, 3, -2, 4]
max 1 (foldr max 0 [5, 3, -2, 4])
max 1 (max 5 (foldr max 0 [3, -2, 4]))
max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
max 1 (max 5 (max 3 (max -2 (max 4 0))))
max 1 (max 5 (max 3 (max -2 4)))
max 1 (max 5 (max 3 4))
max 1 (max 5 4)
max 1 5
5

foldl max 0 [1, 5, 3, -2, 4]
foldl max (max 0 1) [5, 3, -2, 4]
foldl max (max (max 0 1) 5) [3, -2, 4]
foldl max (max (max (max 0 1) 5) 3) [-2, 4]
foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
max (max (max (max (max 0 1) 5) 3) -2) 4
max (max (max (max 1 5) 3) -2) 4
max (max (max 5 3) -2) 4
max (max 5 -2) 4
max 5 4
5

The space complexity of these two functions is the same, linear in 
the input length. The reason for this is because of lazy evaluation.
-}




