quicksort [] = []
quicksort (x:xs) = (quicksort left) ++ [x] ++ (quicksort right)
  where
    left = filter (<x) xs
    right = filter (>=x) xs


mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where
    (left,right) = split xs

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

split xs = (left, right)
  where
    left = take size xs
    right = drop size xs
    size = (length xs) `div` 2

splitProp :: Eq a => [a] -> Bool
splitProp xs = xs == (combine $ split xs)
  where
    combine (left, right) = left ++ right

insertionsort xs = insertionsort' [] xs
  where
    insertionsort' xs [] = xs
    insertionsort' [] (y:ys) = insertionsort' [y] ys 
    insertionsort' (x:xs) (y:ys)
      | y <= x = insertionsort' (y:x:xs) ys 
      | otherwise = insertionsort' (x:(insertionsort' xs [y])) ys


sortProp :: Ord a => ([a] -> [a]) -> [a] -> Bool
sortProp f ls = ordered $ f ls
  where
    ordered [] = True
    ordered [x] = True
    ordered (x:xs)
      | x > (head xs) = False
      | x <=(head xs) = True && ordered xs


flatten xss = [x | xs <- xss, x <- xss]

count [] = 0
count (x:xs) = (count xs) + 1

myFilter _ [] = []
myFilter pred (x:xs)
  | pred x = x:myFilter pred xs
  | otherwise = myFilter pred xs

countIf _ [] = 0
countIf pred (x:xs) = (countIf pred xs) + inc
  where
    inc = if pred x then 1 else 0

flatten2 :: [[a]] -> [a]
flatten2 [] = []
flatten2 (xs:xss) = (flattenInternal xs) ++ (flatten2 xss) 
  where 
    flattenInternal [] = []
    flattenInternal (x:xs) = x:(flattenInternal xs)


