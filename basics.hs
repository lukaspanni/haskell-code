quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs   

testFilterLesser list x = filter (< x) list
testFilterGreater list x = filter (> x) list

squareMap = map(^2)
fib x 
        | x<=1        = 1
        | otherwise   = fib (x-1) + fib (x-2)

reverseList [] = []
reverseList (x:xs) = reverseList (xs) ++ [x]

createInverseList x 
        | x == 1      = 0:[]
        | otherwise   = (x-1:createInverseList (x-1))

-- [0...n-1]
createList x = reverseList (createInverseList x)

squareList x = squareMap (createList x)


main = putStrLn "Hello World"
