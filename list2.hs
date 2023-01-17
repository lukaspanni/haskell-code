import Data.Char 


data List a = Null | Node a (List a) deriving (Show, Eq) 


lfilter p ls = case ls of
  Null -> Null
  (Node v next) -> if p v then Node v (lfilter p next) else lfilter p next

lmap f ls = case ls of
  Null -> Null
  (Node v next) -> Node (f v) (lmap f next)

lfoldl f z ls = case ls of 
  Null -> z
  (Node v next) -> lfoldl f (f z v) next

lfoldr f z ls = case ls of
  Null -> z
  (Node v next) -> f v (lfoldr f z next)



lfiltermap p f ls = case ls of
  Null -> Null
  (Node v next) -> if p v then Node (f v) (lfiltermap p f next) else (lfiltermap p f next)

lor :: List Bool -> Bool 
lor ls = case ls of 
  Null -> False
  (Node True next) -> True 
  (Node v next) -> lor next

land :: List Bool -> Bool 
land ls = case ls of 
  Null -> False 
  (Node v Null) -> v
  (Node False next) -> False 
  (Node v next) -> land next 


lany :: (a -> Bool) -> List a -> Bool
lany f = lor . lmap f 

lall :: (a -> Bool) -> List a -> Bool
lall f = land . lmap f

foldall :: (a -> Bool) -> List a -> Bool
foldall f ls = lfoldl (\z x -> z && f x) True ls

foldany :: (a -> Bool) -> List a -> Bool
foldany f ls = lfoldl (\z x -> z || f x) False ls


fromList xs = case xs of 
  [] -> Null
  (x:xs) -> Node x (fromList xs)


lhead :: Eq a => List a -> Maybe a
lhead node = do
  if node == Null then fail ""
  else 
    let ret (Node v n) = return v 
    in ret node

llast :: List a -> Maybe a 
llast Null = Nothing
llast (Node v n) = case n of
  Null -> Just v
  node -> llast node

lappend Null n = n
lappend (Node v next) n 
  | next == Null = Node v n
  | otherwise = Node v (lappend next n)

lreverse Null = Null
lreverse (Node v next) = lappend (lreverse next) (Node v Null)


type Dict a = List (String, a)

testDict = fromList [([chr x], x) | x <- [65..90]]


dlookup :: String -> Dict a -> Maybe a
dlookup s d = case d of 
  Null -> do {fail "Not found";}
  (Node (k,v) ls) -> do
    if k == s then return v
    else dlookup s ls

dfilter :: (String -> Bool) -> Dict a -> Dict a
dfilter p = lfilter (\(k,v) -> p k) 


dmap :: ((String, a) -> (String, a)) -> Dict a -> Dict a
dmap = lmap


dremove :: String -> Dict a -> Dict a
dremove s = dfilter (/=s) 

dupdate :: String -> a -> Dict a -> Dict a
dupdate s n = dmap (\(k,v) -> if k ==s then (k,n) else (k,v))


dinsert :: String -> a -> Dict a -> Dict a
dinsert s n Null = Node (s,n) Null
dinsert s n (Node (k,v) ls) = Node (k,v) (dinsert s n ls)

dUpdateOrInsert :: String -> a -> Dict a -> Dict a
dUpdateOrInsert s n ls = let contains = dlookup s ls
  in case contains of 
     Nothing -> dinsert s n ls
     Just v -> dupdate s n ls

--should be equal
testDict2 = foldl (\z -> \x -> uncurry dUpdateOrInsert x z) testDict $ map (\x -> ([chr x], x)) [97..(97+25)]
testDict3 = foldr (uncurry dUpdateOrInsert) testDict $ reverse $ map (\x -> ([chr x], x)) [97..(97+25)]




pascal n k
  | k >= n = 1 
  | k == 0 = 1
  | otherwise = pascal (n-1) k + pascal (n-1) (k-1)


printPascal n
  | n < 1 = print "at least one row has to be printed!"
  | otherwise = putStr $ foldl (\z (x,ys) -> z ++ (foldl (\z1 y -> z1 ++ (show (pascal x y) ++ " ")) "" ys) ++ "\n") "" $ map (\x -> (x, [0..x]) ) [0..n]
