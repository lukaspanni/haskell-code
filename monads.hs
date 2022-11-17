import Data.IORef

incIO :: Int -> IO Int
incIO x = do print x
             return (x+1)
incIO2 x = do {print x; return (x+1)}


-- in place update:
-- use pointers like in C => IORef 
incRef :: IORef Int -> IO Int
incRef x = do
  v <- readIORef x    -- dereference, get value of memory location x
  writeIORef x (v+1)  -- update the value of the memory location x
  return (v+1)        -- also return the incremented value

-- in C
-- int inc(int* x){
--  int v = *x;
--  *x = v+1;
--  return v+1;
--  // basically *x = *x+1
-- }

-- in place update without return
incNoReturn :: IORef Int -> IO ()
incNoReturn x = do 
  v <- readIORef x
  writeIORef x (v+1)
  return ()

-- monadic Maybe 
divSafe :: Int -> Int -> Maybe Int
divSafe x y = if y == 0
  then fail "division by zero"
  else return (x `div` y)

-- same function 
divSafe2 x y 
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)

divTwice x y = do
  z <- divSafe x y
  z2 <- divSafe z y
  return z2

-- same as 
divTwice2 x y = case (divSafe x y) of
  Just z -> divSafe z y
  Nothing -> Nothing
