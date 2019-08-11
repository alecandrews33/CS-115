import Control.Monad
import Control.Monad.State
import Data.IORef


-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)
              
-- A.1

factIO :: Integer -> IO Integer

fact n = 
  if n < 0 then error "Input is less than zero"
  else
    do counter <- newIORef n
       total <- newIORef 1
       whileIO
         (do counter' <- readIOref counter
             return (counter' /= 0))
         (do total' <- readIOref total
             counter' <- readIOref counter
             total <- writeIOref (total' * counter')
             counter <- writeIOref (counter' - 1))
       readIOref total
       
-- A.2 

factState :: Integer -> Integer
factState n = 
  if n < 0 then error "Input is less than zero"
  else evalState transform (n, 1) where
    transform :: State (Integer, Integer) Integer
    transform = 
      do whileState (\(counter, _) -> counter /= 0)
        (do (counter, total) <- get
          put (counter - 1, total * counter))
      (_, total) <- get
      return total
      
-- A.3

fibIO :: Integer -> IO Integer
fibIO n = 
  if n < 0 then error "Input less than zero"
  else
    do count <- newIORef n
       first <- newIORef 0
       second <- newIORef 1
       whileIO
         (do count' <- readIORef count
             return (count' /= 0))
         (do count' <- readIORef count
             first' <- readIORef first
             second' <- readIORef second
             count <- writeIORef (count' - 1)
             first <- writeIORef second
             second <- writeIORef (first + second))
       readIORef first
       
-- A.4 

fibState :: Integer -> Integer
fibState n = 
  if n < 0 then error "Input less than zero"
  else evalState transform (0, 1, n) where 
    transform :: (Integer, Integer, Integer) Integer
    transform = 
      do whileState (\(_, _, count) -> count /= 0)
        (do (first, second, count) <- get
          put (second, first + second, count - 1))
      (first, _, _) <- get
      return first
     
     
-- Part B

{-

First, we will derive the >>= operator.

Let's start by assuming we have two functions in the Reader r monad 
with these type signatures:

f :: a -> Reader r b
g :: b -> Reader r c

and we would like to compose them to give a function with the type 
signature: 

h :: a -> Reader r c

Let's rewrite these type signatures in non-monadic form so we can better
see what's going on

f' :: (a, r) -> b
g' :: (b, r) -> c
h' :: (a, r) -> c

We can easily define h' in terms of f' and g':

h' :: (a, r) -> c
h' (x, r) = 
  let y = f' (x, r)
      z = g' (y, r)
  in (z, r)
  
Going back to the original functions f, g, and h, we have

h = f >=> g

which is equivalent to:

h x = f x >>= g

which is equivalent to:

h x = f x >>= \y -> g y

which is equivalent to:

h x = do y <- f x
        g y

Recall our type declarations of f' and g' and curry them:

f' :: (a, r) -> b
g' :: (b, r) -> c

f'' :: a -> r -> b
g'' :: b -> r -> c

in terms of f' and g', we have:

f'' :: a -> r -> b
f'' x r = f (x, r)

g'' :: b -> r -> c
g'' y r = g' (y, r)

Written differently, 

f'' x = \r -> f' (x, r)
g'' y = \r -> g' (y, r)

If we wrap the right-hand sides of f'' and g'' in a Reader constructor,
we have the definitions of f and g in terms of f' and g':

f :: a -> Reader r b
f x = Reader (\r -> f' (x, r))

g :: b -> Reader r c
g y = Reader (\r -> g' (y, r))

We also can define the monadic composition of f and g (h) like:

h :: a -> Reader r c
h x = Reader (\r -> h' (x, r))

Recall:

h = f >=> g

which is equivalent to:

h x = f x >>= g

Reversing the equation, we have 

f x >>= g = h x

Expanding h x, we have:

f x >>= g = Reader (\r -> h' (x, r))

Let us calculate:

f x >>= g
  = Reader (\r -> h' (x, r))
  = Reader (\r -> 
    let y  = f' (x, r)
        z = g' (y, r)
    in (z, r))
  = Reader (\r ->
    let y = f' (x, r) in
      g' (y, r)) 
  = Reader (\r -> 
    let (Reader ff) = f x
      y == ff r
    in g' (y, r))
    
We then can replace g' to get:

f x >>= g 
  = Reader (\r ->
    let (Reader ff) = f x
        y = ff r
        (Reader gg) = g y
    in gg r)
    
Substitute mx for f x, f for g, g for ff, h for gg, x for y to get:

mx >>= f
  = Reader (\r ->
    let (Reader g) = mx
        x = g r
        (Reader h) = f x
    in h r)
    
This is the definition we were given!


Now, we will derive the return function

Monadic functions in the (Reader r) monad have type signatures like:

a -> Reader r b

The non-monadic reader-passing functions have type signatures like:

(a, r) -> b

The identity function in this form would be:

id_reader (x, r) = x
id_reader' x = \r -> x

Written as a function in the (Reader r) monad, this becomes:

id_reader_monad :: a -> Reader r a
id_reader_monad x = Reader (\r -> x)

This is the definition we were given!

-}       
    
