
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

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs

valid :: DB -> Bool
valid db = (noDuplicateStudentIds db) && (allStudentsValid db)
  where
    noDuplicateStudentIds db = not (hasDuplicates (map (\(_, id, _) -> id) db))
    allStudentsValid db = all (==True) [not (hasDuplicates cs) | (_,_,cs) <- db]

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
query1 db id = [cs | (_,i,cs) <- db, i==id]!!0

-- TASK 2
{-
Given a database and a course, find all students
taking this course.
-}
query2 :: DB -> Integer -> [String]
query2 db c = [n | (n,_,cs) <- db, elem c cs]


-- TASK 3
{-
Given a database, sort the database (in non-decreasing order)
according to the name of students. -> sort by id instead of name
-}
-- sorting using quicksort
sortDB :: DB -> DB
sortDB [] = []
sortDB (x:xs) = (sortDB left) ++ [x] ++ (sortDB right)
  where
    left = filter (\(_,i,_) -> i <= (getId x)) xs
    right = filter (\(_,i,_) -> i > (getId x)) xs
    getId (_,i,_) = i

sortDB' :: DB -> DB
sortDB' [] = []
sortDB' (x:xs) = (sortDB' left) ++ [x] ++ (sortDB' right)
  where
    left = filter (\(_,_,cs) -> (length cs) <= (getCourses x)) xs
    right = filter (\(_,_,cs) -> (length cs) > (getCourses x)) xs
    getCourses (_,_,cs) = length cs

sortDB'' :: (Student -> Student -> Ordering) -> DB -> DB
sortDB'' o [] = []
sortDB'' o (x:xs) = (sortDB'' o left) ++ [x] ++ (sortDB'' o right)
  where
    left = filter (\y -> o y x == LT || o y x == EQ) xs 
    right = filter (\y -> o y x == GT) xs

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
      then EQ
      else GT

sortName = sortDB'' cmpName

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

merge :: DB -> DB -> DB
merge db [] = db
merge db ((n,i,cs):ys) = merge mdb ys
  where
    mdb = (n,i,(mergeCourses cs cs2)):(filter (\(_,i2,_) -> i /= i2) db)
    cs2 = [c | (_,i2,css) <- db, c <- css, i == i2]



mergeCourses :: [Integer] -> [Integer] -> [Integer]
mergeCourses cs cs2 = removeDuplicates (cs++cs2)
  where 
    removeDuplicates [] = []
    removeDuplicates (x:xs)
      | elem x xs = removeDuplicates xs
      | otherwise = x:(removeDuplicates xs)


merge2 :: DB -> DB -> DB
merge2 db [] = db
merge2 db (y:ys) = merge (insertMerge db y) ys

insertMerge :: DB -> Student -> DB
insertMerge db (n2,i2,cs2)
  | (length others) == (length db) = (n2,i2,cs2):db
  | otherwise = [(n,i,(mergeCourses cs cs2)) | (n,i,cs) <- db, i == i2]++others
  where 
    others = [(n,i,cs) | (n,i,cs) <- db, i /= i2]

