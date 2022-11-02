data BinaryTree a = Null | Node a (BinaryTree a) (BinaryTree a) deriving (Show)


instance Eq a => Eq (BinaryTree a) where
  (==) (Node a Null Null) (Node b Null Null) = a == b
  (==) (Node a b c) (Node d e f) = a == d && b == e && c == f

instance Functor BinaryTree where
  fmap f Null = Null
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

class Val a where
  val :: a b -> b

instance Val BinaryTree where
  val (Node a _ _) = a

class Height a where
  height :: a b -> Integer

instance Height BinaryTree where
  height Null = 0
  height (Node _ l r) = 1 + max (height l) (height r)

class Count a where
  count :: a -> Integer

instance Count (BinaryTree a) where
  count Null = 0
  count (Node _ l r) = 1 + (count l) + (count r)


treeSum :: Num a => BinaryTree a -> a
treeSum Null = 0
treeSum (Node a l r) = a + (treeSum l) + (treeSum r)

condTreeSum :: Num a => (a -> Bool) -> BinaryTree a -> a
condTreeSum _ Null = 0
condTreeSum p (Node a l r)
  | p a = a + (condTreeSum p l) + (condTreeSum p r)
  | otherwise = (condTreeSum p l) + (condTreeSum p r)

treeProduct :: Num a => BinaryTree a -> a
treeProduct Null = 1
treeProduct (Node a l r) = a * (treeProduct l) * (treeProduct r)

getLeftmostLeaf :: BinaryTree a -> a
getLeftmostLeaf (Node a Null Null) = a
getLeftmostLeaf (Node _ a _) = getLeftmostLeaf a

isLeaf :: BinaryTree a -> Bool
isLeaf (Node a Null Null) = True
isLeaf (Node _ _ _) = False

isNull :: BinaryTree a -> Bool
isNull Null = True
isNull _ = False

leftChild :: BinaryTree a -> BinaryTree a
leftChild (Node _ l _) = l

rightChild :: BinaryTree a -> BinaryTree a
rightChild (Node _ _ r) = r


insert :: BinaryTree a -> a -> BinaryTree a
insert t i
  | isNull t = Node i Null Null
  | height (leftChild t) > height (rightChild t) = Node (val t) (leftChild t) (insert (rightChild t) i)
  | otherwise = Node (val t) (insert (leftChild t) i) (rightChild t)

insertSorted :: Ord a => BinaryTree a -> a -> BinaryTree a
insertSorted Null i = Node i Null Null
insertSorted (Node a l r) i
  | a > i = Node a (insertSorted l i) r
  | otherwise = Node a l (insertSorted r i)


filterTree :: Ord a =>  (a -> Bool) -> BinaryTree a -> BinaryTree a
filterTree p t = toSortedTree (filter p (toListInorder t))

toListInorder :: BinaryTree a -> [a]
toListInorder Null = []
toListInorder (Node a l r) = (toListInorder l) ++ [a] ++ (toListInorder r)

toListPreorder :: BinaryTree a -> [a]
toListPreorder Null = []
toListPreorder (Node a l r) = a:(toListPreorder l) ++ (toListPreorder r)

toListPostorder :: BinaryTree a -> [a]
toListPostorder Null = []
toListPostorder (Node a l r) = (toListPostorder l) ++ (toListPostorder r) ++ [a]

toTree :: [a] -> BinaryTree a
toTree xs = foldl (\t -> \x -> insert t x) Null xs

toSortedTree :: Ord a => [a] -> BinaryTree a
toSortedTree xs = foldl (\t -> \x -> insertSorted t x) Null xs 

-- "cheats" by building a new tree
reSortTree :: Ord a => BinaryTree a -> BinaryTree a
reSortTree tree = toSortedTree (toListInorder tree) 

