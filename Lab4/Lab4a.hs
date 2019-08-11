module Lab4a where

-- A.1

myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = (putChar c) >> (myPutStrLn cs)

-- A.2

greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- A.3
{- 
-- Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Hello, "
  putStr name
  putStrLn "!"


Method 1
greet2 :: IO ()
greet2 =
  putStr "Enter your name: " >>
  getLine >>= \name ->
  (putStr "Hello, " >> putStr name >> putStrLn "!")
  
Method 2
greet2 :: IO ()
greet2 = getLine >>= 
  \x -> case x of 
   name -> (putStr "Hello, " >> putStr name >> putStrLn "!")
   _ -> fail "Pattern match failure in do expression"
   
No pattern match will fail, so these two will function the same. 

-}

-- A.4

{-

-- Need to import this to get the definition of toUpper:
import Data.Char

-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"
  
Method 1 
greet3 :: IO ()
greet3 = 
  putStr "Enter your name: " >> 
  getLine >>= \(n:ns) ->
    let name = toUpper n : ns in 
      (putStr "Hello, " >> putStr name >> putStrLn "!")
  
Method 2
greet3 :: IO () 
greet3 = 
  putStr "Enter your name: " >>
  getLine >>= \x -> case x of 
    (n:ns) -> let name = toUpper n : ns in 
      (putStr "Hello, " >> putStr name >> putStrLn "!")
    _ -> fail "Pattern match failure in do expression"
    
The more complex method does have an effect here. User input of nothing,
"the empty string", would not be handled properly using method 1.
-}
    
  
    
    

