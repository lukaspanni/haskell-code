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
decProduct ls n = ([ (x1,x2,x3) | (x1,x2,x3) <- ls, x1 == n ], [ (x1,x2-1,x3) | (x1,x2,x3) <- ls, x1 == n, x2-1==0 ])

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
  | otherwise = False || hasDuplicateNames xs

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



