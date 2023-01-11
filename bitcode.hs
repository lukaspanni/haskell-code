data Tree = Leaf Char | Node Tree Tree

data Code = Zero | One deriving (Show, Eq)

type Table = [(Char, [Code])]

codeTable :: Tree -> Table
codeTable (Leaf c) = [(c, [])]
codeTable (Node l r) = 
  let tablel = codeTable l
      tabler = codeTable r
  in (map (\(c,cd) -> (c, Zero:cd)) tablel ++ map (\(c,cd) -> (c, One:cd)) tabler)


t1 = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

t1Code = codeTable t1

retrieve :: Table -> Char -> [Code]
retrieve [] _ = []
retrieve t c = [cc | (x,cd) <- t, x==c, cc <- cd]


retrieve' t c = (map (\(c,code) -> code) $ filter (\(c1,_) -> c1 == c) t)!!0


encode :: Tree -> [Char] -> [Code]
encode t [] = []
encode t (x:xs) = (retrieve (codeTable t) x) ++ (encode t xs)

encode' t = flatten . map (retrieve (codeTable t)) 
  where 
    flatten [] = []
    flatten (x:xs) = x++(flatten xs)

encode'' t = foldl (\z -> \c -> z ++ (retrieve (codeTable t) c)) [] 

valid :: Tree -> Bool
valid t = hasNoDups $ [c | (c,_) <- codeTable t]
  where
    hasNoDups [] = True
    hasNoDups (x:xs)
      | elem x xs = False
      | otherwise = hasNoDups xs 


decode :: Tree -> [Code] -> [Char]
decode t [] = []
decode t ls = decode' t (decodeC t ls)
  where
    decode' t (c,[]) = [c]
    decode' t (c,ls) = c:(decode' t (decodeC t ls))
    decodeC (Leaf c) ls = (c,ls)
    decodeC (Node l r) (x:xs)
      | x == One = decodeC r xs
      | otherwise = decodeC l xs


simplerDecode :: Tree -> [Code] -> [Char]
simplerDecode t ls = decodeI t t ls
  where
    decodeI t (Leaf c) [] = [c]
    decodeI t (Leaf c) ls = c:(decodeI t t ls)
    decodeI t (Node l r) (x:xs)
      | x == One = decodeI t r xs
      | otherwise = decodeI t l xs


evenSimplerDecode :: Tree -> [Code] -> [Char]
evenSimplerDecode t ls = decode' t ls
  where
    decode' (Leaf c) [] = [c]
    decode' (Leaf c) ls = c:(decode' t ls)
    decode' (Node l r) (x:xs)
      | x == One = decode' r xs
      | otherwise = decode' l xs
