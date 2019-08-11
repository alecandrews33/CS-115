module Lab3ab where

-- A.1
{-
data Nat = Zero | Succ Nat

instance Eq Nat where
  Zero   == Zero     = True
  Succ n == Succ m   = n == m
  _      == _        = False
	
instance Show Nat where 
  show Zero = "Zero"
  show (Succ Zero) = "Succ Zero"
  show (Succ x) = "Succ (" ++ show x ++ ")"
 -}
 
-- A.2
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- A.3
instance Ord Nat where
  Zero   <= _      = True
  _      <= Zero = False
  Succ n <= Succ m = (n <= m)
  
{- We could have had Haskell automatically derive Ord since the
constructor puts the values in the correct order. Zero is less 
than any Succ, and each subsequent Succ makes the numbers larger 
and larger. -}

-- A.4
data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)
  
instance Eq SignedNat where 
  Neg Zero == Pos Zero = True
  Pos Zero == Neg Zero = True
  Neg _ == Pos _ = False
  Pos _ == Neg _ = False
  Neg n == Neg m = n == m
  Pos n == Pos m = n == m
  
instance Ord SignedNat where
  Neg _ <= Pos _ = True
  Pos x <= Neg y 
    | x == Zero && y == Zero = True
    | otherwise = False
  Neg n <= Neg m = n >= m
  Pos n <= Pos m = n <= m
  
{- We can't let Haskell automatically derive Ord since this would make
all Neg values less than all Pos values. However, Neg Zero is equal to 
Pos Zero, so we must enforce this property manually -}
  
-- A.5
natAdd :: Nat -> Nat -> Nat
natAdd Zero x = x
natAdd y Zero = y
natAdd (Succ x) (Succ y) = Succ (Succ (natAdd x y))

natSub :: Nat -> Nat -> Nat
natSub x Zero = x
natSub (Succ x) (Succ y) 
  | x == y = Zero
  | otherwise = natSub x y
natSub _ _ = error("Cannot subtract larger Nat from smaller Nat") 

natMult :: Nat -> Nat -> Nat
natMult _ Zero = Zero 
natMult Zero _ = Zero
natMult (Succ x) (Succ y) = natAdd (Succ x) (natMult (Succ x) y)

signedNatAbs :: SignedNat -> SignedNat
signedNatAbs (Neg n) = Pos n
signedNatAbs m@(Pos _) = m

signedNatSigNum :: SignedNat -> SignedNat
signedNatSigNum (Neg Zero) = Pos Zero
signedNatSigNum m@(Pos Zero) = m
signedNatSigNum (Pos _) = Pos (Succ Zero)
signedNatSigNum (Neg _) = Neg (Succ Zero)

natFromInteger :: Integer -> Nat
natFromInteger 0 = Zero
natFromInteger x 
  | x > 0 = Succ (natFromInteger (x - 1))
  | otherwise = Succ (natFromInteger (x + 1))

signedNatFromInteger :: Integer -> SignedNat
signedNatFromInteger x | x > 0 = Pos (natFromInteger x)
                       | x < 0 = Neg (natFromInteger x)
                       | otherwise = Pos Zero
                    
signedNatAdd :: SignedNat -> SignedNat -> SignedNat
signedNatAdd (Pos x) (Neg y) | x > y = Pos (natSub x y)
                             | x < y = Neg (natSub y x)
                             | otherwise = Pos Zero
signedNatAdd (Neg x) (Pos y) | x > y = Neg (natSub x y)
                             | x < y = Pos (natSub y x)
                             | otherwise = Pos Zero
signedNatAdd (Pos x) (Pos y) = Pos (natAdd x y)
signedNatAdd (Neg x) (Neg y) = Neg (natAdd x y)

signedNatSub :: SignedNat -> SignedNat -> SignedNat 
signedNatSub (Pos x) (Neg y) = Pos (natAdd x y)
signedNatSub (Neg x) (Pos y) = Neg (natAdd x y)
signedNatSub (Pos x) (Pos y) | x > y = Pos (natSub x y)
                             | x < y = Neg (natSub y x)
                             | otherwise = Pos Zero
signedNatSub (Neg x) (Neg y) | x > y = Neg (natSub x y)
                             | x < y = Pos (natSub y x)
                             | otherwise = Pos Zero
                             
signedNatMult :: SignedNat -> SignedNat -> SignedNat 
signedNatMult (Pos x) (Pos y) = Pos (natMult x y)
signedNatMult (Neg x) (Neg y) = Pos (natMult x y)
signedNatMult (Pos x) (Neg y) = Neg (natMult x y)
signedNatMult (Neg x) (Pos y) = Neg (natMult x y)

signedNatNegate :: SignedNat -> SignedNat
signedNatNegate x = (Pos Zero) - x

instance Num SignedNat where
  (+) = signedNatAdd 
  (-) = signedNatSub 
  (*) = signedNatMult 
  negate = signedNatNegate  
  abs = signedNatAbs 
  signum = signedNatSigNum
  fromInteger = signedNatFromInteger
                     
-- A.6

signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger (Pos (Succ x)) = 1 + (signedNatToInteger (Pos x))
signedNatToInteger (Neg (Succ x)) = -1 + (signedNatToInteger (Neg x))

-- A.7
{- 
Having to put Pos and Neg everywhere is ugly and redundant. Also, there 
is no definitive way to represent 0, since Pos Zero and Neg Zero are 
going to equate to the same thing. 

data UnaryInteger = Prev | Zero | Succ

This has a specific value for Zero, and we are able to represent 
positive and negative numbers using a unary approach.

 -}

-- A.8 
factorial :: (Num a, Ord a) => a -> a

factorial x | x == 1  || x == 0 = 1
            | x > 1 = x * factorial (x - 1)           
            | otherwise = error("Can not have negative inputs")

-- B.1.1

{- 
>#< is non-associative. The reason for this is because it returns a 
string, which is different from its argument types, which are both 
integers. Thus, one cannot create a chained operator expression that 
will work for this operator, and it is infix.
-}

-- B.1.2
{-
+| can be either left-associative or right-associative, but we will 
declare it as infixl. Consider the example: (5 +| 7) +| 9. This goes to 
2 +| 9, and then finally returns with 1. If we hadn't used this operator
we could just add up 5, 7, and 9, and see that the result is 21, whose 
last digit is indeed 1. Therefore, making it left-associative preserved 
the correctness of the operation, and we could make longer chained 
operator expressions as well, and we could associate to the right 
and correctness would still be preserved. 
-}

-- B.1.3

{- 
&< must be left associative (infixl). Consider the example: 
([1, 2] &< 3) &< 4. This would yield [1, 2, 3] &< 4, and finally
[1, 2, 3, 4]. This operator cannot be right-associative, since it 
returns a list, and a list is the first argument (so that return value 
would need to be on the left).
-}

-- B.1.4

{-
>&& must be right-associative (infixr). Consider the example:
1 >&& (2 >&& [3, 4, 5]). This gives 1 >&& [2, 2, 3, 4, 5] and finally
[1, 1, 2, 2, 3, 4, 5]. This operator returns a list, and its right 
argument is a list, so it must be right-associative. 
-}

-- B.2
{- 
This operator could technically be either left or right associative 
and it would type check. The reason for this is because both arguments
and its return value are all integers. Although the return value is 
an integer, it does not represent the same thing that the arguments 
were representing. It is the length of a sum, not the sum itself, so 
chaining these operations together would give unexpected results. 
Given this, this operation should be made non-associative with infix
-}


