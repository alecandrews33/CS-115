module RedBlackTree where

-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either empty (a "leaf") or a tree node with a color,
-- two branches (both of which are red-black trees), and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show
 
 
-- A.1 
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node _ left value right) | x < value = member x left
                                   | x == value = True
                                   | otherwise = member x right
                              
   
-- A.2
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ l value r) = (toList l) ++ (value : (toList r))

-- A.3

insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t) 
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right) 
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t = 
      Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance Black (Node Red l1 e1 (Node Red l2 e2 r2)) e t = 
      Node Red (Node Black l1 e1 l2) e2 (Node Black r2 e t)
    balance Black t e (Node Red (Node Red l1 e1 r1) e2 r2) = 
      Node Red (Node Black t e l1) e1 (Node Black r1 e2 r2)
    balance Black t e (Node Red l1 e1 (Node Red l2 e2 r2)) = 
      Node Red (Node Black t e l1) e1 (Node Black l2 e2 r2)
    balance color l e r = Node color l e r  -- no balancing needed
    
-- A.4
fromList :: Ord a => [a] -> Tree a
fromList lst = foldr insert Leaf lst

-- A.5
minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ l _ r) = 1 + min (minDepth l) (minDepth r)

maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ l _ r) = 1 + max (maxDepth l) (maxDepth r)

-- A.6
invariant1Helper :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool
invariant1Helper Leaf _ _ = True
invariant1Helper (Node _ l e r) Nothing Nothing = (invariant1Helper l 
  Nothing (Just e)) && (invariant1Helper r (Just e) Nothing)
invariant1Helper (Node _ l e r) (Just low) Nothing = (e > low)
  && (invariant1Helper l (Just low) (Just e)) && (invariant1Helper r
  (Just e) Nothing)
invariant1Helper (Node _ l e r) Nothing (Just high) = (e < high)
  && (invariant1Helper l Nothing (Just e)) && (invariant1Helper r 
  (Just e) (Just high))
invariant1Helper (Node _ l e r) (Just low) (Just high) = (e > low) &&
  (e < high) && (invariant1Helper l (Just low) (Just e)) && 
  (invariant1Helper r (Just e) (Just high))

testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 t = invariant1Helper t Nothing Nothing


-- A.7
testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node Black l _ r) = (testInvariant2 l) && 
                                    (testInvariant2 r)
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False
testInvariant2 (Node Red _ _ (Node Red _ _ _ )) = False
testInvariant2 (Node Red l@(Node Black _ _ _ ) _ r@(Node Black _ _ _))
  = (testInvariant2 l) && (testInvariant2 r)
testInvariant2 _ = True
    
-- A.8
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = 
      (leafCounts left (n + 1)) ++ (leafCounts right (n + 1))
    leafCounts (Node Red left _ right) n = 
      (leafCounts left n) ++ (leafCounts right n)
    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False                       
 
    
-- PART B

-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList

-- B.1
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset s1 s2 = all (\x -> member x s2) (toList s1)

-- B.2
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = (isSubset s1 s2) && (isSubset s2 s1)

-- B.3

union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr insert s2 (toList s1) 

-- B.4
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = foldr (\x r -> if member x s1 
                            then insert x r
                            else r) empty (toList s2)
                            
-- B.5 
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if member x s2 
                                  then r 
                                  else insert x r) empty (toList s1)

