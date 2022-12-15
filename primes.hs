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
