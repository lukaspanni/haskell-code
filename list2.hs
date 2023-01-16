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

type Dict a = [(String, a)] 

testDict = [([chr x], x) | x <- [65..90]]

dlookup :: String -> Dict a -> Maybe a
dlookup s d = case d of 
  [] -> do {fail "Not found";}
  ((k,v):xs) -> do
    if k == s then return v
    else dlookup s xs


dupdate :: String -> a -> Dict a -> Dict a
dupdate s v [] = []
dupdate s v ((k,v1):xs)
  | s == k = (k,v):xs
  | otherwise = (k,v1):(dupdate s v xs)

dremove :: String -> Dict a -> Dict a
dremove s [] = []
dremove s ((k,v):xs)
  | s == k = xs
  | otherwise = (k,v):(dremove s xs)
