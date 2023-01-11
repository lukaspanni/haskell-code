

-------------------------------------------------------
-- Extended Haskell programming exercise
-- Topic: functions over lists
-- Author: Martin Sulzmann
-------------------------------------------------------



-- A student is represented by her name, student id and
-- a list of the courses the student is taking


type Student = (String, Integer, [Integer])
type DB = [Student]



-- TASK 0
{-
Databases must be consistent.
We say the database is consistent if there're no multiple entries
of students, and no multiple entries of courses per students
 For example, the below databases are *inconsistent*
-}

incons1 = [("Jack", 111, [141, 252, 141])]
incons2 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252])]

{-
Your task is to implement the following function
which returns True if the database is valid (consistent),
otherwise the function returns False.
-}
removeDuplicates [] = [] 
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

hasNoDuplicates xs = xs == removeDuplicates xs

valid :: DB -> Bool
valid xs = (hasNoDuplicates (map (\(n,_,_) -> n) xs)) && (hasNoDuplicates (map (\(_,i,_) -> i) xs)) && (all [hasNoDuplicates c | (_,_,c) <- xs])
  where
    all [y] = y == True
    all (False:ys) = False 
    all (_:ys) = all ys


-- EXTENSION TO TASK 0
{-
Extension: We strengthen the notion of consistency.
In addition, we require that there shall by no duplicate student id's.
For example,
-}

incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]
cons1 =  [("Jane", 111, [141]), ("Jack", 112, [141, 252])]


-- FROM HERE ON, WE WILL ASSUME THAT ALL DATABASES ARE CONSISTENT !!!

testdb = cons1 ++ [("A", 14, [141]), ("Lukas", 12, [141, 252, 126]), ("Peter", 42, [141])]

-- TASK 1
{-
Given a database and a student id, we're looking for the list of
courses of this particular student.
-}
query1 :: DB -> Integer -> [Integer]
query1 db id = [ c |(n,i,cs) <- db, c <- cs, i==id]


-- TASK 2
{-
Given a database and a course, find all students
taking this course.
-}
query2 :: DB -> Integer -> [String]
query2 db c = [n | (n,i,cs) <- db, elem c cs] 



-- TASK 3
{-
Given a database, sort the database (in non-decreasing order)
according to the name of students.
-}

-- sorting using quicksort
sortDB :: DB -> DB
sortDB [] = []
sortDB ((n,i,c):xs) = (sortDB left) ++ [(n,i,c)] ++ (sortDB right)
  where
    left = filter (\(n1,_,_) -> n1 < n) xs
    right = filter (\(n1,_,_) -> n1 >= n) xs

sortDB' :: DB -> DB
sortDB' [] = []
sortDB' ((n,i,cs):xs) = (sortDB' left) ++ [(n,i,cs)] ++ (sortDB' right)
  where
    left = filter (\(_,_,cs1) -> (length cs1) < (length cs)) xs
    right = filter (\(_,_,cs1) -> (length cs1) >= (length cs)) xs

sortDB'' :: (Student -> Student -> Ordering) -> DB -> DB
sortDB'' cmp [] = []
sortDB'' cmp (x:xs) = (sortDB'' cmp left) ++ [x] ++ (sortDB'' cmp right)
  where
    left = filter (\y -> cmp y x == LT) xs
    right = filter (\y -> cmp y x /= LT) xs

{-
Extension1:
Provide a function sortDB' which sorts the database according to the number of courses a student is taking

Extension2:
Provide a function sortDB'' which is parameterized in the actual comparing relation which determines the sorting order
For example:
 Given 
-}

cmpName :: Student -> Student -> Ordering
cmpName (n1, _, _) (n2, _, _) =
 if n1 < n2
 then LT
 else if n1 == n2
      then GT
      else EQ

{-
Then you can define

 sortDB = sortDB'' cmpName

-}


-- TASK 4
{-
Given two databases, merge them to obtain one (consistent) database
 Example:

 merge [("Jane", 112, [141, 353])] [("Jane", 112, [141, 252])]
    => [("Jane", 112, [141, 252, 353])]

-}

mergedb1 = [("Jane", 112, [141, 353])]
mergedb2 = [("Jane", 112, [141, 252])]

mergeCourses :: [Integer] -> [Integer] -> [Integer]
mergeCourses [] cs = cs
mergeCourses cs [] = cs
mergeCourses (c1:cs1) cs2 = c1:mergeCourses cs1 (filter (\x -> x /= c1) cs2)

merge :: DB -> DB -> DB
merge [] db = db
merge db [] = db
merge (s1:db1) db2 = (update s1 db2):(merge db1 (filtered s1 db2))
  where
    filtered (_,i,_) xs = filter (\(_,i1,_) -> i /= i1) xs
    update (n,i,c) xs = mergeStudent (n,i,c) (filter (\(_,i1,_) -> i==i1) xs)
    mergeStudent (n,i,c) [(n1,i1,c1)] = (n,i,(mergeCourses c c1))


merge2 [] db = db
merge2 db [] = db
merge2 (s1:db1) db2 = update s1 ++ (merge2 db1 (filtered s1 db2))
  where 
    update (n,i, c) = [(n,i,(mergeCourses2 c c1)) | (_,i1,c1) <- db2, i1 == i]
      where 
        mergeCourses2 cs1 cs2
          | null cs1 = cs2
          | null cs2 = cs1
          | otherwise = (head cs1):mergeCourses (tail cs1) (filter (\x -> x /= (head cs1)) cs2)
    filtered (_,i,_) db = [ (n,i1,c) | (n,i1,c) <- db, i1 /= i]


    
