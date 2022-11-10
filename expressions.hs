data Exp = One | Zero | ETrue | EFalse | Plus Exp Exp | Mult Exp Exp | Or Exp Exp | And Exp Exp deriving (Eq)

instance Show Exp where
    show One = "1"
    show Zero = "0"
    show ETrue = "true"
    show EFalse = "false"
    show (Plus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mult e1 e2) =  show e1 ++ " * " ++ show e2
    show (Or e1 e2) = "(" ++ show e1 ++ " || " ++ show e2 ++ ")"

eval :: Exp -> Maybe (Either Int Bool)
eval One = Just (Left 1)
eval Zero = Just (Left 0)
eval ETrue = Just (Right True)
eval EFalse = Just (Right False)

eval (Plus e1 e2) =
  let r1 = eval e1
      r2 = eval e2
  in case (r1, r2) of
    ((Just (Left x)), (Just (Left y)))  -> Just (Left (x+y))
    (_,_)                               -> Nothing
eval (Mult e1 e2) =
  let r1 = eval e1
      r2 = eval e2
  in case (r1, r2) of
    ((Just (Left x)), (Just (Left y)))  -> Just (Left (x*y))
    (_,_)                               -> Nothing
eval (Or e1 e2) =
  case (eval e1) of
    Nothing -> Nothing
    (Just (Right True))   -> Just (Right True)
    (Just (Right False))  -> case (eval e2) of
                              Nothing               -> Nothing
                              (Just (Right True))   -> Just (Right True)
                              _                     -> Just (Right False)
    _                     -> Nothing

eval (And e1 e2) =
  case (eval e1) of
    Nothing               -> Nothing
    (Just (Right True))   -> case (eval e2) of
                              Nothing             -> Nothing
                              (Just (Right True)) -> Just (Right True)
                              _                   -> Just (Right False)
    (Just (Right False))  -> Just (Right False)
    _                     -> Just (Right False)



data Type = TInt | TBool deriving (Show)

typecheck :: Exp -> Maybe Type
typecheck ETrue = Just TBool
typecheck EFalse = Just TBool
typecheck Zero = Just TInt
typecheck One = Just TInt

typecheck (Plus e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just TInt, Just TInt)  -> Just TInt
    (_, _)        -> Nothing
typecheck (Mult e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just TInt, Just TInt)  -> Just TInt
    (_, _)        -> Nothing
typecheck (Or e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just TBool, Just TBool)  -> Just TBool
    (_, _)          -> Nothing
typecheck (And e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just TBool, Just TBool)  -> Just TBool
    (_, _)          -> Nothing

-- needs typechecked Expression, but is then simpler
evalT :: Exp -> Either Int Bool
evalT One = Left 1
evalT Zero = Left 0
evalT ETrue = Right True
evalT EFalse = Right False
evalT (Plus e1 e2) =
    case (evalT e1, evalT e2) of
       (Left i1, Left i2) -> Left (i1 + i2)
evalT (Mult e1 e2) =
    case (evalT e1, evalT e2) of
       (Left i1, Left i2) -> Left (i1 * i2)
evalT (Or e1 e2) =
     case (evalT e1, evalT e2) of
         (Right True, Right b2) -> Right True
         (Right False, Right b2) -> Right b2

simplify :: Exp -> Exp
simplify e
  | e == e2 = e
  | otherwise = simplify e2
  where 
    e2 = simp e 
-- "ultra simplification" for only zero and one
simp :: Exp -> Exp
simp (Plus Zero e1) = e1 
simp (Plus e1 Zero) = e1
simp (Plus e1 e2) = Plus (simp e1) (simp e2)
simp (Mult One e1) = e1
simp (Mult e1 One) = e1
simp (Mult Zero e1) = Zero
simp (Mult e1 Zero) = Zero
simp (Mult e1 e2) = Mult (simp e1) (simp e2)
simp (Or ETrue e1) = ETrue
simp (Or e1 ETrue) = ETrue
simp (Or e1 e2) = Or (simp e1) (simp e2)
simp (And EFalse e1) = EFalse
simp (And e1 EFalse) = EFalse
simp (And e1 e2) = And (simp e1) (simp e2)
simp e = e

--typesafe exp
data ExpT a where
   One_Exp :: ExpT Int
   Zero_Exp :: ExpT Int
   ETrue_Exp :: ExpT Bool
   EFalse_Exp :: ExpT Bool
   Plus_Exp :: ExpT Int -> ExpT Int -> ExpT Int
   Mult_Exp :: ExpT Int -> ExpT Int -> ExpT Int
   Or_Exp :: ExpT Bool -> ExpT Bool -> ExpT Bool
   And_Exp :: ExpT Bool -> ExpT Bool -> ExpT Bool


evalExp :: ExpT a -> a
evalExp One_Exp = 1
evalExp Zero_Exp = 0
evalExp ETrue_Exp = True
evalExp EFalse_Exp = False
evalExp (Plus_Exp e1 e2) = evalExp e1 + evalExp e2
evalExp (Mult_Exp e1 e2) = evalExp e1 * evalExp e2
evalExp (Or_Exp e1 e2) = evalExp e1 || evalExp e2
evalExp (And_Exp e1 e2) = evalExp e1 && evalExp e2
