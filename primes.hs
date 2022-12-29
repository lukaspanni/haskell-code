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

