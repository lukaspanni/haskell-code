import Data.Char 


data Bits = Zero | One deriving (Eq, Show)
data HTree a = Leaf Float a | Node Float (HTree a) (HTree a) deriving (Eq, Show)

type Code = [Bits]

class Probability a where
  prob :: a -> Float

instance Probability (HTree a) where
  prob (Leaf p _) = p
  prob (Node p _ _) = p 

flipTree :: HTree a -> HTree a
flipTree t = case t of
  (Leaf p a) -> Leaf p a
  (Node p l r) -> Node p (flipTree r) (flipTree l)

cmpPropabilities (_,p1) (_,p2) = cmp p1 p2 

cmp p1 p2
  | p1 < p2 = LT
  | p1 > p2 = GT
  | otherwise = EQ

cmpNodes :: HTree a -> HTree a -> Ordering
cmpNodes x y = cmp (prob x) (prob y)

sort _ [] = []
sort c (x:xs) = (sort c left) ++ [x] ++ (sort c right)
  where
    left = filter (\y -> c y x == LT) xs
    right = filter (\y -> c y x /= LT) xs

sortProb = sort cmpPropabilities


unique [] = []
unique (x:xs) 
  | elem x xs = unique xs
  | otherwise = x:(unique xs)

count x xs = foldl (\z y -> if y == x then z + 1 else z + 0 ) 0 xs

flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

insertNodeSorted n [] = [n]
insertNodeSorted n (x:xs)
  | prob n > prob x = x:(insertNodeSorted n xs)
  | otherwise = n:x:xs

-----

buildTree :: Eq a => [a] -> HTree a
buildTree cs = flipTree $ combine $ map (\(c,p) -> Leaf p c) $ sortProb $ computeProbabilities cs
  where
    combine :: [HTree a] -> HTree a
    combine [x] = x 
    combine (x:xs) = 
      let node = (Node (prob x + (prob $ head xs)) x $ head xs)
      in combine $ insertNodeSorted node (tail xs)


computeProbabilities :: Eq a => [a] -> [(a, Float)]
computeProbabilities [] = []
computeProbabilities cs = computeProbabilities' $ unique cs
  where
    computeProbabilities' [] = []
    computeProbabilities' (x:xs) = (x, ((count x cs) / (fromIntegral $ length cs))) : (computeProbabilities' xs)


codeTable :: HTree a -> [(a, Code)]
codeTable (Leaf p c) = [(c, [Zero])]
codeTable t = codeTable' [] t
  where
    codeTable' cd (Leaf p c) = [(c, cd)]
    codeTable' cd (Node p l r) = (codeTable' (cd++[Zero]) l) ++ (codeTable' (cd++[One]) r)

extractCode :: Eq a => a -> [(a, Code)] -> Code 
extractCode x t = flatten [cd | (c,cd) <- t, c == x ]

encode :: Eq a => HTree a -> [a] -> Code
encode t s = flatten $ map (\x -> extractCode x (codeTable t)) s

encodeToBinary t s = encode t s 
encodeText s = flatten $ map (\x -> toBinary (ord x)) s

codeEfficiency t s = fromIntegral (length $ encodeText s) / fromIntegral (length $ encodeToBinary t s) 

decode :: Eq a => HTree a -> Code -> [a]
decode t cd = decode' cd t
  where
    decode' cd (Leaf p c) = c:(decode' cd t)
    decode' [] t = []
    decode' (c:cs) (Node p l r)
      | c == Zero = decode' cs l
      | otherwise = decode' cs r

toByteChunks :: Code -> [Code]
toByteChunks xs 
  | len == 0 = []
  | len < 8 = [(take (8 - len) $ repeat Zero) ++ xs]
  | otherwise = toByteChunks (take (len - 8) xs) ++ [(drop (len - 8) xs)]
  where
    len = length xs 

fill :: Code -> Code
fill cd = (take (8 - ((length cd) `mod` 8)) $ repeat Zero) ++ cd 

toBinary :: Int -> Code
toBinary n = fill $ toBinary' n
  where 
    toBinary' 0 = [Zero]
    toBinary' n 
      | n == 1 = [One]
      | (n `mod` 2) == 0 = (toBinary' (n `div` 2)) ++ [Zero]
      | otherwise = (toBinary' (n `div` 2)) ++ [One]

fromBinary :: Code -> Int
fromBinary [] = 0
fromBinary (x:xs)
  | x == One = round (2**fromIntegral (length xs)) + (fromBinary xs)
  | otherwise = fromBinary xs
