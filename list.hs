data LinkedList a = Null | Node a (LinkedList a) deriving (Show)

class Val a where
  val :: a b -> b

class Count a where
  count :: a -> Integer

class Ordered a where
  isOrdered :: a -> Bool

instance Val LinkedList where
  val (Node a _) = a

instance Count (LinkedList a) where
  count Null = 0
  count (Node _ n) = 1 + (count n)

instance Ord a => Ordered (LinkedList a) where
  isOrdered Null = True
  isOrdered (Node a Null) = True
  isOrdered (Node a n) = a < val n && isOrdered n

instance Functor LinkedList where
  fmap f Null = Null
  fmap f (Node a n) = Node (f a) (fmap f n)


toList :: LinkedList a -> [a]
toList Null = []
toList (Node a n) = a:(toList n)

fromList :: [a] -> LinkedList a
fromList [] = Null
fromList (x:xs) = Node x (fromList xs) 

sortedFromList :: Ord a => [a] -> LinkedList a
sortedFromList xs = foldl (\l -> \x -> insertSorted l x) Null xs 

insert :: LinkedList a -> a -> LinkedList a
insert Null i = Node i Null
insert (Node a n) i = Node a (insert n i)

insertSorted :: Ord a => LinkedList a -> a -> LinkedList a
insertSorted Null i = Node i Null
insertSorted (Node a n) i
  | i <= a  = Node i (Node a n)
  | otherwise = Node a (insertSorted n i)

filterList :: (a -> Bool) -> LinkedList a -> LinkedList a
filterList _ Null = Null
filterList p (Node a n)
  | p a = Node a (filterList p n)
  | otherwise = filterList p n


