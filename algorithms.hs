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


