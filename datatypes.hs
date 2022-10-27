-- Enumerations
data RGBColor = Red
  | Green
  | Blue
  deriving (Eq, Ord)

-- show in german
instance Show RGBColor where
  show Red = "rot"
  show Green = "grün"
  show Blue = "blau"


-- :type Red => Red :: RGBColor
toHex :: RGBColor -> String
toHex Red = "ff0000"
toHex Green = "00ff00"
toHex Blue = "0000ff"


data ProgrammingLanguages = Haskell | C deriving (Eq)

instance Ord ProgrammingLanguages where 
  C <= Haskell = True
  Haskell <= C = False


-- Maybe/Either
-- -> constructors with parametric arguments (a,b,...) -> any type, same type on the other side
-- data Maybe a = Just a | Nothing => just the same type as input or nothing
-- data Either a b = Left a | Right b => one of those types
type Result = Either Int String -- => result is an integer or a string (on failure)
failSafeDiv :: Int -> Int -> Result
failSafeDiv n 0 = Right "division by zero"
failSafeDiv n m = Left (n `div` m)



-- define a list type:
--  empty OR
--  non-empty: head:tail => (Cons a (List a)) -> Cons: build list from head and tail
-- derives from Show and Eq -> allows to print and compare lists
data List a = Null | Cons a (List a) deriving (Show)
-- EBNF: List ::= Null | number List; number ::= (0\1\2\3\4\5\6\7\8\9)+
-- every context-free grammar can be used to describe a haskell data type

-- Eq a: a has to have an instance of Eq to allow this instance of Eq for List of a's
instance Eq a => Eq (List a) where
  (==) Null Null = True
  (==) Null _ = False
  (==) _ Null = False
  (==) (Cons x xs) (Cons y ys) = x==y && (xs == ys)


nfilter :: (a -> Bool) -> List a -> List a
nfilter p Null = Null
nfilter p (Cons x xs)
  | p x = Cons x (nfilter p xs)
  | otherwise = nfilter p xs


-- custom list to builtin
convert :: List a -> [a]
convert Null = []
convert (Cons x xs) = x:(convert xs)

cheatFilter :: (a -> Bool) -> List a -> List a
cheatFilter p ls = convertBack (filter p (convert ls))

-- builtin to custom List
convertBack :: [a] -> List a
convertBack [] = Null
convertBack (x:xs) = Cons x (convertBack xs)



