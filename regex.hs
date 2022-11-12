data Regex a = Epsilon
           | Symbol a 
           | Concat (Regex a) (Regex a)
           | Alternative (Regex a) (Regex a) 
           | Star (Regex a)
           | Plus (Regex a)
           deriving (Eq)

instance Show a => Show (Regex a)  where
  show Epsilon = "ε"
  show (Symbol c) = show c
  show (Concat r1 r2) = "(" ++ show r1 ++ show r2 ++ ")"
  show (Alternative r1 r2) = show r1 ++ "|" ++ show r2
  show (Star r) = "(" ++ show r ++ ")*"
  show (Plus r) = "(" ++ show r ++ ")+"


buildAlternative :: [Regex a] -> Regex a
buildAlternative [r] = r
buildAlternative (r:rs) = Alternative r (buildAlternative rs)

buildAlternativeFromSymbolList :: [a] -> Regex a
buildAlternativeFromSymbolList xs = buildAlternative (map Symbol xs)

nullable :: (Regex a) -> Bool
nullable Epsilon = True
nullable (Symbol _) = False
nullable (Concat r1 r2) = nullable r1 && nullable r2
nullable (Alternative r1 r2) = nullable r1 || nullable r2
nullable (Star _) = True
nullable (Plus r) = nullable r

simplify :: Eq a => (Regex a) -> (Regex a)
simplify r 
  | r == r2 = r
  | otherwise = simplify r2
  where r2 = simplifyInternal r

simplifyInternal :: Eq a => (Regex a) -> (Regex a)
simplifyInternal (Concat Epsilon r) = simplifyInternal r
simplifyInternal (Concat r Epsilon) = simplifyInternal r
simplifyInternal (Concat r1 r2) = Concat (simplifyInternal r1) (simplifyInternal r2)
simplifyInternal (Alternative r1 r2)
  | r1 == r2 = simplifyInternal r1
  | otherwise = Alternative (simplifyInternal r1) (simplifyInternal r2)
simplifyInternal (Star r) = Star (simplifyInternal r)
simplifyInternal (Plus r) = Plus (simplifyInternal r)
simplifyInternal r = r

-- check if any part of the string matches
match :: Eq a => [a] -> (Regex a) -> Bool
match [] r = nullable r
match s Epsilon = True
match (c:s) (Symbol c1)
  | c == c1 = True
  | otherwise = match s (Symbol c1)
match s (Concat r1 r2) = match s r1 && match s r2
match s (Alternative r1 r2) = match s r1 || match s r2
match s (Plus r) = match s r
match s (Star r) = match s (Alternative Epsilon (Plus r))
