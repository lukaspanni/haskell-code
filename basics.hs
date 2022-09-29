quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs   


testFilterLesser list x = filter (< x) list
testFilterGreater list x = filter (> x) list

main = putStrLn "Hello World"

