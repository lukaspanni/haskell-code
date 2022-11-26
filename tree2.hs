data Tree a = Null | Leaf a | Node (Tree a) (Tree a) deriving (Show)


instance Eq a => Eq (Tree a) where
  (==) (Leaf x) (Leaf y) = x == y
  (==) (Node l r) (Node l1 r1) = l== l1 && r == r1 
  (==) _ _ = False


treeSum :: Num a => Tree a -> a
treeSum Null = 0
treeSum (Leaf x) = x
treeSum (Node l r) = (treeSum l) + (treeSum r)


toList :: Tree a -> [a]
toList Null = []
toList (Leaf x) = [x]
toList (Node l r) = (toList l) ++ (toList r)


insertSorted :: Ord a => Tree a -> a -> Tree a
insertSorted Null i = Leaf i
insertSorted (Leaf a) i
  | i > a = Node (Leaf a) (Leaf i)
  | otherwise = Node (Leaf i) (Leaf a)
insertSorted (Node l r) i
  | (smallest r) > i = Node (insertSorted l i) r 
  | otherwise = Node l (insertSorted r i)
    where 
      -- assumes tree without null
      smallest (Leaf x) = x
      smallest (Node l r) = smallest l

sortedFromList :: Ord a => [a] -> Tree a
sortedFromList [] = Null
sortedFromList (x:xs) = insertSorted (sortedFromList xs) x


class Sort a where
  sort :: a -> a

class Height a where
  height :: a -> Int

instance Ord a => Sort (Tree a) where
  sort t = sortedFromList $ toList t

instance Height (Tree a) where
  height t = case t of 
    Null      -> 0
    Leaf _    -> 1
    Node l r  -> 1 + (max (height l) (height r))

leafCount :: Tree a -> Int
leafCount t = case t of
  Null      -> 0
  Leaf _    -> 1
  Node l r  -> (leafCount l) + (leafCount r)


treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Null = Null
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)


treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter p Null = Null
treeFilter p (Leaf x)
  | p x = Leaf x
  | otherwise = Null
treeFilter p (Node l r) = Node (treeFilter p l) (treeFilter p r)


treeFilter2 p t = case (p,t) of
  (p, Null) -> Null
  (p, (Leaf a)) -> if p a then (Leaf a) else Null
  (p, (Node l r)) -> Node (treeFilter2 p l) (treeFilter2 p r)



