import DatabaseTypes


testProducts = ProductTable [Product (1, "MacBook Air", 1099.99), Product (2, "Surface Pro", 999.99)]
testUsers = UserTable [User (1, "lukaspanni", "lukas@lukaspanni.de")]
testOrders = OrderTable [Order (1,1), Order (2,1)]
testOrderItems = OrderItemTable [OrderItem (1,1,1,1, 1099.99), OrderItem (2,1,2,1,999.99), OrderItem (3, 2, 1, 1, 1099.99)]
testDb = Database testUsers testProducts testOrders testOrderItems


getUniqueUsers :: UserTable -> [UserRow]
getUniqueUsers (UserTable []) = []
getUniqueUsers (UserTable ((User (uid,n,m)):xs)) = (User (uid,n,m)):getUniqueUsers (UserTable (filter (\(User (id, _, _)) -> id /= uid) xs))

registerUser :: FullDB -> (String, String) -> FullDB
registerUser (Database (UserTable users) products orders orderItems) (name, mail) = Database (insertUser users) products orders orderItems
  where 
    insertUser :: [UserRow] -> UserTable
    insertUser [] = UserTable [User (1, name, mail)]
    insertUser us = UserTable $ sort ((User ((maxId us) + 1, name, mail)):us) orderUser
    maxId :: [UserRow] -> Int
    maxId [] = 0
    maxId ((User (id, _, _)):xs) = max id $ maxId xs


select :: [a] -> (a -> Bool) -> [a]
select [] _ = []
select (x:xs) p = case p x of
  True -> x:(select xs p)
  False -> select xs p

selectTable :: Simplify a b => a -> (b -> Bool) -> [b]
selectTable t p = select (simplify t) p


getProductsFromOrder :: FullDB -> Int -> [ProductRow]
getProductsFromOrder (Database _ products orders orderItems) oid = selectTable products (\(Product (id, _, _)) -> (elem id orderItemIds))
  where
    orderItemIds = map (\(OrderItem (_, _, pid, _, _)) -> pid) $ selectTable orderItems (\(OrderItem (id, oid1, _, _, _)) -> oid1 == oid)
