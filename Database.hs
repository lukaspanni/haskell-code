

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
valid xs = (hasNoDuplicates (map (\(n,_,_) -> n) xs)) && (hasNoDuplicates (map (\(_,i,_) -> i) xs))&& (all [hasNoDuplicates c | (_,_,c) <- xs])
  where
    all [y] = y == True
    all (y:ys)
      | y == True = True && (all ys)
      | otherwise = False



-- EXTENSION TO TASK 0
{-
Extension: We strengthen the notion of consistency.
In addition, we require that there shall by no duplicate student id's.
For example,
-}

incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]
cons1 =  [("Jane", 111, [141]), ("Jack", 112, [141, 252])]


-- FROM HERE ON, WE WILL ASSUME THAT ALL DATABASES ARE CONSISTENT !!!


-- TASK 1
{-
Given a database and a student id, we're looking for the list of
courses of this particular student.
-}
query1 :: DB -> Int -> [Int]
query1 = error "Your code"


-- TASK 2
{-
Given a database and a course, find all students
taking this course.
-}
query2 :: DB -> Int -> [String]
query2 = error "Your code"



-- TASK 3
{-
Given a database, sort the database (in non-decreasing order)
according to the name of students.
-}
sortDB :: DB -> DB
sortDB = error "Your code"

{-
Extension1:
Provide a function sortDB' which sorts the database according to the number of courses a student is taking

Extension2:
Provide a function sortDB'' which is parameterized in the actual comparing relation which determines the sorting order
For example:
 Given

cmpName :: Student -> Student -> Ordering
cmpName (n1, _, _) (n2, _, _) =
 if n1 < n2
 then LT
 else if n1 == n2
      then GT
      else EQ

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

merge :: DB -> DB -> DB
merge = error "Your code"
