import Test.QuickCheck

isPrime x
  | x == 1 = True
  | otherwise = isNotDividable x (x-1)
  where
    isNotDividable :: Int -> Int -> Bool
    isNotDividable x y
      | y == 1 = True
      | isInt ((fromIntegral x) / (fromIntegral y)) == True = False
      | otherwise = isNotDividable x (y-1)
isInt x = x == fromIntegral (round x)

genPrimes n = filter isPrime [1..n]
genNextPrime n = (filter isPrime [n+1..n*2])!!0 

sort [] = []
sort (x:xs) = (sort left) ++ x:(sort right)
  where
    left = filter (<=x) xs
    right = filter (>x) xs

flattenTuples ts = flatten [(\(a,b) -> a:b:[]) x | x <- ts]
flatten xss = [x | xs <- xss, x <- xs]

factorize :: Int -> [Int]
factorize x
  | x == 0 = []
  | isPrime x = [x]
  | otherwise = sort $ factorize' x 2 -- 2 = genNextPrime 1
  where
    factorize' :: Int -> Int -> [Int]
    factorize' x p
      | p >= x = []
      | isInt ((fromIntegral x) / (fromIntegral p)) = (factorize (x `div` p)) ++ [p] ++ (factorize (x `mod` p))
      | otherwise = factorize' x (genNextPrime p)

getFactors x = filter (\y -> isInt ((fromIntegral x)/(fromIntegral y))) (genPrimes x)

getNumberOfDividers = foldl (*) 1 . map (\(a,b) -> b+1) . counts . factorize
  where
    counts :: Eq a => [a] -> [(a,Int)]
    counts xs = map (\x -> (x, (count x xs))) $ uniqueElements xs
    count x xs = length $ filter (==x) xs
    uniqueElements [] = []
    uniqueElements (x:xs) = if elem x xs then uniqueElements xs else x:(uniqueElements xs)



--- easier implementations
dividers n = [d | d <- [1..n `div` 2], n `mod` d == 0]
isPrime2 n = length (dividers n) == 1

primeFactors n = [p | p <- dividers n, isPrime2 p]

factorize2 x 
  | x == 0 = []
  | isPrime x = [x]
  | otherwise = sort $ factorize' x (primeFactors x) 
  where
    factorize' :: Int -> [Int] -> [Int]
    factorize' 1 _ = []
    factorize' x (p:ps) 
      | x `mod` p == 0 = p: (factorize' (x `div` p) (p:ps))
      | otherwise = factorize' x ps


factorizeProp n = if n < 1 then True else (foldl (*) 1 $ factorize2 n) == n
