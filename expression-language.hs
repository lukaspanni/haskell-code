data Expression = Value (Either Int Bool) | Plus Expression Expression | OR Expression Expression | Greater Expression Expression deriving (Show)

instance Eq Expression where
  (==) exp1 exp2 = (eval exp1) == (eval exp2)


eval :: Expression -> Either Int Bool
eval (Value x) = x
eval (Plus exp1 exp2) = 
  case (r1, r2) of
    ((Left x), (Left y)) -> Left $ x + y
    _                    -> error "cannot add Int and Bool"
    where
      r1 = eval exp1
      r2 = eval exp2
eval (OR exp1 exp2) =
  case (r1, r2) of
    ((Right True), _)      -> Right True
    (_, (Right True))      -> Right True 
    ((Right x), (Right y)) -> Right $ x || y
    _                      -> error "Cannot use OR for Int and Bool" 
    where
      r1 = eval exp1
      r2 = eval exp2
eval (Greater exp1 exp2) = 
  case (r1, r2) of 
    ((Left x), (Left y)) -> Right $ x > y
    _                    -> error "Cannot compare Int and Bool"
    where
      r1 = eval exp1
      r2 = eval exp2

