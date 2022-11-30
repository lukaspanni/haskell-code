{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies #-}  
module DatabaseTypes where


newtype UserRow = User (Int, String, String) deriving (Show, Eq) -- UserId, Name, Mail
newtype ProductRow = Product (Int, String, Float) deriving (Show, Eq) -- ProductId, Name, Price
newtype OrderRow = Order (Int, Int) deriving (Show, Eq) -- OrderId, UserId
newtype OrderItemRow = OrderItem (Int, Int, Int, Int, Float) deriving (Show, Eq) -- OrderItemId, OrderId, ProductId, Quantity, Price

newtype UserTable = UserTable [UserRow] deriving (Show, Eq)
newtype ProductTable = ProductTable [ProductRow] deriving (Show, Eq)
newtype OrderTable = OrderTable [OrderRow] deriving (Show, Eq)
newtype OrderItemTable = OrderItemTable [OrderItemRow] deriving (Show, Eq)

data FullDB = Database UserTable ProductTable OrderTable OrderItemTable deriving (Show, Eq)


sort :: [a] -> (a -> a -> Ordering) -> [a]
sort [] _ = []
sort (x:xs) o = (left xs) ++ [x] ++ (right xs)
  where 
    left ls = filter (\y -> o y x == LT) ls 
    right ls = filter (\y -> o y x /= LT) ls 

orderUser :: UserRow -> UserRow -> Ordering
orderUser (User (id1, _, _)) (User (id2, _, _))
  | id1 < id2 = LT
  | id1 == id2 = EQ
  | otherwise = GT

orderProduct :: ProductRow -> ProductRow -> Ordering
orderProduct (Product (id1, _, _)) (Product (id2, _, _))
  | id1 < id2 = LT
  | id1 == id2 = EQ
  | otherwise = GT

orderOrder :: OrderRow -> OrderRow -> Ordering
orderOrder (Order (id1, _)) (Order (id2, _))
  | id1 < id2 = LT
  | id1 == id2 = EQ
  | otherwise = GT

orderOrderItem :: OrderItemRow -> OrderItemRow -> Ordering
orderOrderItem (OrderItem (id1, _, _, _, _)) (OrderItem (id2, _, _, _, _))
  | id1 < id2 = LT
  | id1 == id2 = EQ
  | otherwise = GT


class Simplify a b | a -> b where
  simplify :: a -> [b]

instance Simplify UserTable UserRow where
  simplify (UserTable users) = users

instance Simplify ProductTable ProductRow where
  simplify (ProductTable products) = products

instance Simplify OrderTable OrderRow where
  simplify (OrderTable orders) = orders

instance Simplify OrderItemTable OrderItemRow where
  simplify (OrderItemTable orderItems) = orderItems
