import System.Random
import Data.Char

count :: String -> Int
count [] = 0
count (c:cs)
  | c == ' ' = count (skipBlanks cs)
  | otherwise = 1 + (count (skipWord cs))
  where
    skipBlanks = skip (== ' ')
    skipWord = skip (/= ' ')

skip :: (Char -> Bool) -> String -> String
skip p [] = []
skip p (c:cs)
  | p c = skip p cs
  | otherwise = c:cs


splitWords :: String -> [String]
splitWords [] = []
splitWords cs = extractWord:(splitWords $ skipBlanks $ skipWord cs)
  where 
    skipWord = skip (/= ' ')
    skipBlanks = skip (== ' ')
    extractWord = extractTo (==' ') [] cs


extractTo :: (Char -> Bool) -> String -> String -> String
extractTo p res [] = res
extractTo p res (c:cs)
  | p c = res
  | otherwise = extractTo p (res ++ [c]) cs

type Assertion = IO ()
assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqual what expected given 
  | expected == given = putStrLn ("[OK]" ++ what)
  | otherwise = fail (what ++ ": assertEqual failed. Expected" ++ show expected ++ ", given: " ++ show given)

test_count = do assertEqual "Test1" 0 (count "")
                assertEqual "Test2" 1 (count "Test ")
                assertEqual "Test3" 2 (count "Test Test")
-- problems: how many tests are needed?
-- -> can we generate test cases?

-- "QuickCheck light":

class Arbitrary a where
  arbitrary :: IO a

elements :: [a] -> IO a
elements xs = do i <- randomIO :: IO Int
                 return (xs !! (i `mod` (length xs)))

vector :: Arbitrary a => Int -> IO [a]
vector 0 = return []
vector n 
  | n > 0 = do x <- arbitrary 
               xs <- vector (n-1)
               return (x:xs)
  | otherwise = error "length has to be > 0"

instance Arbitrary Char where
  arbitrary = do x <- elements [0..255]
                 return (chr x)

instance Arbitrary Bool where
  arbitrary = do x <- elements [True, False]
                 return x

instance Arbitrary Int where
  arbitrary = do x <- randomIO :: IO Int
                 return x


instance Arbitrary a => Arbitrary [a] where
  arbitrary = do {n <- elements [1..10]; xs <- vector n; return xs}

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do b <- randomIO
                 if b
                    then do {l <- arbitrary; return (Left l);}
                 else do {r <- arbitrary; return (Right r);}


genString2 :: IO [Char]
genString2 = (arbitrary :: IO String)

genStrings :: IO [String]
genStrings = (arbitrary :: IO [String])


-- Property based testing
--  -> Identify properties (invariants/assertions) that must be satisfied (because we do not know what the actual output should be)

-- simple property: result has to be >= 0
prop1 :: String -> Bool
prop1 s = count s >= 0

-- another property: reversed string has the same number of words
prop2 :: String -> Bool
prop2 s = count s == count (reverse s)

-- now automated test generation is possible to check the properties
quickCheck :: (Show t, Arbitrary t) => (t -> Bool) -> IO ()
quickCheck prop = go 100
  where go 0 = putStrLn "++++ OK"
        go n = do x <- arbitrary
                  if prop x
                    then go (n-1)
                  else do {putStrLn "**** Failed: "; print x;}

-- property based testing does not substitute unit-tests! -> but improves chances of finding bugs

