import Test.QuickCheck
import System.Random

data Set a = Empty | Set [a]

instance Show a => Show (Set a) where
  show Empty = "{}"
  show (Set s) = "{" ++ show' s ++ "}"
    where
      show' [] = ""
      show' [x] = show x
      show' (x:xs) = show x ++", " ++ show' xs 

instance Eq a => Eq (Set a) where
  (==) Empty Empty = True
  (==) Empty (Set []) = True
  (==) (Set []) Empty = True
  (==) (Set ls) (Set ls2) = (foldl (\acc -> \x -> acc && elem x ls2) True ls) && (foldl (\acc -> \x ->acc && elem x ls) True ls2)

contains :: Eq a => Set a -> a -> Bool
contains Empty _ = False
contains (Set ls) n = elem n ls

union :: Eq a => Set a -> Set a -> Set a
union Empty s = s
union s Empty = s
union (Set s1) (Set s2) = toSet (s1 ++ s2)


intersect :: Eq a => Set a -> Set a -> Set a
intersect Empty _ = Empty
intersect _ Empty = Empty 
intersect (Set s1) (Set s2) = toSet ([x | x <- (s1++s2), (elem x s1) && (elem x s2)])


difference :: Eq a => Set a -> Set a -> Set a
difference Empty _ = Empty
difference s Empty = s
difference (Set s1) (Set s2) = (Set [x | x <- s1, not $ elem x s2 ])


toSet :: Eq a => [a] -> Set a 
toSet [] = Empty
toSet ls
  | hasDuplicates ls = Set (removeDuplicates ls)
  | otherwise = Set ls

hasDuplicates ls = (removeDuplicates ls) /= ls
removeDuplicates [] = []
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x:(removeDuplicates xs)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do {
      e <- choose (True, False);
      if e then return Empty;
      else do {
        n <- choose (1,100);
        ls <- vectorOf n arbitrary;
        return (toSet ls);
      }
    }

isSet :: Eq a => Set a -> Bool
isSet Empty = True
isSet (Set s) = not (hasDuplicates s)


sameDifferenceEmpty :: Eq a => Set a -> Bool 
sameDifferenceEmpty s = difference s s == Empty  

sameEqual :: Eq a => Set a -> Bool 
sameEqual s = s == s

