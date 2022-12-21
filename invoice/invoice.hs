module Invoice where

import Data.List

data Customer = Customer { name :: String, street :: String, zipCode :: String, city :: String, phone :: String }

instance Show Customer where 
  show (Customer name street zipCode city phone) = name ++ " | " ++ street ++ ", " ++ zipCode ++ " " ++ city ++ " | " ++ phone


prettyPrint :: Customer -> [Int] -> String
prettyPrint c [] = prettyPrint c $ replicate 5 0 
prettyPrint (Customer name street zipCode city phone) colWidth = (printCol name (colWidth!!0)) ++ " | " ++ (printCol street (colWidth!!1)) ++ ", " ++ (printCol zipCode (colWidth!!2)) ++ " " ++ (printCol city (colWidth!!3)) ++ " | " ++ (printCol phone (colWidth!!4))
  where 
    printCol :: String -> Int -> String
    printCol str colWidth
      | colWidth > (length str) = str ++ (replicate (colWidth - length str) ' ')
      | otherwise = str


splitFields :: String -> [String]
splitFields [] = []
splitFields (x:xs)
  | x == ',' = [] : splitFields xs
  | xs == [] = [[x]]
  | otherwise = (x:head (splitFields xs)) : tail (splitFields xs)


trim :: String -> String
trim = trimEnd . trimStart
  where 
    trimStart = dropWhile (==' ')
    trimEnd = reverse . trimStart . reverse


parseCustomers :: String -> [Customer]
parseCustomers = map parseCustomer . map (map trim) . map splitFields . drop 1 . lines
  where  
    parseCustomer entry = Customer { name = entry!!0, street = entry!!1, zipCode = entry!!2, city = entry!!3, phone = entry!!4 }


printCustomers :: [Customer] -> IO ()
printCustomers customers = mapM_ (\(i,x) -> putStrLn (show  i ++ ") " ++ prettyPrint x (colWidths))) (zip [1..] customers) 
  where
    colWidths = map maximum $ transpose $ map (map length) $ map (\x -> [name x, street x, zipCode x, city x, phone x]) customers 


main :: IO ()
main = do 
  customerData <- readFile "customer-data.csv"
  let customers = parseCustomers customerData
  putStrLn ("Select a customer to print an invoice for")
  printCustomers customers
  putStr ("Enter customer number (1-" ++ show (length customers) ++"): ")
  let inputNumber = readLn :: IO Int
  customerNumber <- inputNumber
  putStrLn $ "Selected Customer " ++ show customerNumber ++ ")  " ++ prettyPrint (customers!!(customerNumber-1)) [] 

