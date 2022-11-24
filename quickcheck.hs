import System.Random
import Test.QuickCheck

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


prop1 :: String -> Bool
prop1 s = count s >= 0

prop2 :: String -> Bool
prop2 s = count s == count (reverse s)


data CustomString = MkStr String deriving (Show, Eq)

instance Arbitrary CustomString where
  arbitrary =
    do {n <- elements [1..10];
        ws <- vectorOf n word;
        ws2 <- mapM (\w -> do b1 <- elements [0..2]
                              b2 <- elements [0..2]
                              return $ blanks b1 ++ w ++ blanks b2) ws;
        return $ MkStr $ concat ws2; }
      where 
        blanks n = take n (repeat ' ')
        letter = elements $ ['a'..'z'] ++ ['A'..'Z']
        word = do {n <- elements [1..5]; vectorOf n letter}

toString :: CustomString -> String
toString (MkStr s) = s

fromString :: String -> CustomString
fromString = MkStr

reverseCustomString = fromString . reverse . toString
appendCustomString x y = fromString $ (toString x) ++ (toString y)

countCustomString :: CustomString -> Int
countCustomString = count . toString

prop1_c :: CustomString -> Bool
prop1_c s = countCustomString s >= 0

prop2_c :: CustomString -> Bool
prop2_c s = countCustomString s == countCustomString (reverseCustomString s)




data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do left <- arbitrary
                 right <- arbitrary
                 x <- arbitrary
                 arbitraryTree <- elements [Leaf x, Node left right]
                 return arbitraryTree



genTrees n = generate (vectorOf n (arbitrary :: Gen (Tree Int)))
