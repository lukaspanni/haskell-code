data Tree a = Leaf a | Node a (Tree a) (Tree a) | Null deriving (Show)

sorted :: Tree Int -> Bool
sorted Null = True
sorted (Leaf a) = True
sorted (Node v l r) 
  | v >= val l && v <= val r = sorted l && sorted r
  | otherwise = False
  where 
    val (Leaf v) = v
    val Null = 0
    val (Node v l r) = v


toList :: Tree a -> [a]
toList Null = []
toList (Leaf v) = [v]
toList (Node v l r) = (toList l) ++ (v:(toList r))


height :: Tree a -> Int
height Null = 0
height (Leaf _) = 1
height (Node v l r) = 1+ (myMax (height l) (height r))
  where
    myMax a b
      | a > b = a
      | otherwise = b 


fromList :: Ord a => [a] -> Tree a
fromList = foldl (insertSorted) Null

insertSorted :: Ord a => Tree a -> a -> Tree a
insertSorted Null i = Leaf i
insertSorted (Leaf v) i 
  | i > v = Node v Null (Leaf i)
  | otherwise = Node v (Leaf i) Null
insertSorted (Node v l r) i
  | i > v = Node v l (insertSorted r i)
  | otherwise = Node v (insertSorted l i) r


data Path = LeftChild | RightChild | ThisNode deriving (Show)

pathTo :: Ord a => Tree a -> a -> [Path]
pathTo Null _ = []
pathTo (Leaf v) s 
  | v == s = [ThisNode]
  | otherwise = []
pathTo (Node v l r) s 
  | v == s = [ThisNode]
  | s > v = RightChild:(pathTo r s)
  | otherwise = LeftChild:(pathTo l s)

codeTable :: Ord a => Tree a -> [(a, [Path])]
codeTable t = map (\x-> (x, (pathTo t x))) $ toList t
