import Data.Typeable
import Data.IORef
import Test.QuickCheck

quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs   

-- quickCheck stuff
property1 :: Ord a => [a] -> Bool
property1 xs = length xs == length (quicksort xs) -- list size should not change after sort


testFilterLesser list x = filter (< x) list
testFilterGreater list x = filter (> x) list

-- list stuff
squareMap = map(^2)
--squareMap = let square x = x*x
--  in map square
--squareMap = map square
--  where 
--    square x = x * x
--squareMap = map (\x -> x * x)


myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)


incAllPositive xs = [(+1) x | x <- xs, x > 0] 
decAllNegative xs = map (\x -> x-1) (filter (<0) xs)

reverseList [] = []
reverseList (x:xs) = reverseList (xs) ++ [x]

createInverseList x 
  | x == 1      = 0:[]
  | otherwise   = (x-1:createInverseList (x-1))

-- [0...n-1]
createList x = reverseList (createInverseList x)

squareList x = squareMap (createList x)


listSum [] = 0
listSum (x:xs) = x + listSum xs

listProduct xs
  | null xs = 1
  | otherwise = head xs * listProduct xs

customHead (x:_) = x

len [] = 0
len (x:xs) = 1 + len xs

avg xs = (listSum xs) / (len xs)


fib x 
  | x<=1        = 1
  | otherwise   = fib (x-1) + fib (x-2)


data ProgrammingLanguage = Haskell | Java | C deriving Show

instance Eq ProgrammingLanguage where
  (==) Haskell Haskell = True
  (==) Java Java = True
  (==) C C = True
  (==) _ _ = False

instance Ord ProgrammingLanguage where
  (<=) C Haskell = True
  (<=) Java Haskell = True
  (<=) Java C = True
  (<=) Java Java = True
  (<=) C C = True
  (<=) _ _ = False

-- function stuff
add :: Num a => (a, a) -> a
add (x,y) = x + y

plus :: Num a => a -> a -> a -- equals a -> (a -> a);
plus x = \y -> x + y -- basically same as plus x y = x + y



-- in haskell all functions can be used like an operator using `function`
plus1_2 = 1 `plus` 2
-- also operators can be used like a function using (operator)
plus2_3 = (+) 2 3
-- -> allows partial operator application just like with functions
inc = (1+) -- equals (+) 1; operator as function applied to one, second operand still missing
inc4 = inc 4

apply f x = f x
dec = apply (\x -> x - 1)



main = do {
        putStrLn "Hello World";
        print "Input number";
        num <- getLine;
        n <- newIORef (read num :: Int);
        x <- readIORef n; -- read value from memory at n; x is a Int, n is an IORef!
        print ("Type x: " ++ show (typeOf x) ++ " Type n: " ++ show (typeOf n));
        writeIORef n (fib x); -- write new value to memory at n
        x <- readIORef n; -- read (new) value from memory at n
        print x;
        }
