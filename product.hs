type ProductName = String
type ProductPrice = Float
type Product = (ProductName, Int, ProductPrice)
type ProductList = [Product]

productCost :: Product -> ProductPrice
productCost x = (\(_,num,price) -> (fromIntegral num) * price) x

lessEqualProduct (_,a1,a2) (_,b1,b2) 
  | a1 == b1 = a2 < b2
  | otherwise = a1 < b1

overallCost :: ProductList -> ProductPrice
overallCost [] = 0
overallCost (x:xs) = (productCost x) + overallCost xs

-- overallCost ps = foldl (\acc -> \(_,q,p) -> acc+ (p*q)) 0 ps

decProduct :: ProductList -> ProductName -> (ProductList, ProductList)
decProduct ls n = (all, zeroStock)
  where
    all = [if n1==n then (n,s-1,p) else (n1,s,p) | (n1,s,p) <- ls]
    zeroStock = filter (\(n1,s,p) -> s==0) all

updatePrice :: ProductName -> ProductPrice -> ProductList -> ProductList
updatePrice n p ls = map (upPrice n p) ls
  where
    upPrice n p (x1,x2,x3)
      | n == x1 = (x1,x2,p)
      | otherwise = (x1,x2,x3)

removeDuplicates [] = []
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

hasDuplicateNames :: ProductList -> Bool
hasDuplicateNames ((x,_,_):xs)
  | null xs = False
  | length (filter (\(y,_,_) -> y==x) xs) > 0 = True
  | otherwise = hasDuplicateNames xs

--hasDuplicateNames xs = not (names == (removeDuplicates names)) 
--  where
--    names = map (\(n,_,_) -> n) xs

combineProductList :: ProductList -> ProductList -> ProductList
combineProductList [] ys = ys
combineProductList ((x1,x2,x3):xs) ys = combineProduct (x1,x2,x3) ys ++ combineProductList xs (filteredYs)
  where
    combineProduct (x1,x2,x3) ys = if length combined > 0 then combined else [(x1,x2,x3)]
      where combined = [(x1,x2+y2,(max x3 y3)) | (y1,y2,y3) <- ys, y1 == x1]
    filteredYs = filter (\(y1,_,_) -> not (y1 ==x1)) ys


combineProductList' xs [] = xs
combineProductList' xs (y:ys) = combineProductList (insert xs y) ys
  where
    insert [] y = [y]
    insert ((n1,s1,p1):xs) (n2,s2,p2)
      | n1 == n2 = (n1, s1+s2, (max p1 p2)):xs
      | otherwise = (n1,s2,p1):(insert xs y)
