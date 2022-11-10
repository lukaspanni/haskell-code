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

reverseList :: LinkedList a -> LinkedList a
reverseList Null = Null
reverseList (Node a n) = append (reverseList n) (Node a Null)

append :: LinkedList a -> LinkedList a -> LinkedList a
append Null n = n
append n Null = n
append (Node a Null) list = Node a list
append (Node a n) list = Node a (append n list)

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


quicksort :: Ord a => LinkedList a -> LinkedList a
quicksort Null = Null
quicksort (Node a n) = (quicksort smallerOrEqual) `append` ((Node a Null) `append` (quicksort larger))
  where 
    larger = filterList (> a) n
    smallerOrEqual = filterList (<= a) n


binarySearch :: Ord a => [a] -> a -> Int
binarySearch xs v = binarySearchInternal xs v 0 ((length xs)-1)

binarySearchInternal :: Ord a => [a] -> a -> Int -> Int -> Int
binarySearchInternal xs v lower higher
  | mid < 0 || mid == (length xs) || lower == higher = -1
  | v == xs!!mid = mid
  | v >= xs!!mid = binarySearchInternal xs v (mid+1) higher
  | otherwise = binarySearchInternal xs v lower (mid-1)
  where 
    mid = lower + ((higher - lower) `div` 2)


binarySearch2 :: Ord a => [(a, b)] -> a -> b
binarySearch2 [(a, b)] s = b
binarySearch2 ls s
  | index midElem == s = val midElem
  | index midElem <= s = binarySearch2 (drop mid ls) s
  | otherwise = binarySearch2 (take mid ls) s
  where
    mid = (length ls) `div` 2
    index (i,_) = i
    val (_,v) = v
    midElem = ls!!mid

