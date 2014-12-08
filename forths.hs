add :: [Int] -> [Int]
add (x:x2:xs) = x + x2 : xs
add _ = error "not enough data on stack"

data Expr =
    FInt Int
  | FCall ([Int] -> [Int])

eval :: [Expr] -> [Int]
eval []            = []
eval ((FInt n) : xs) = n : eval xs
eval ((FCall f) : xs) = f $ eval xs

main = do
  putStrLn $ show $ eval $ reverse [FInt 3, FInt 2, FCall add, FInt 100, FCall add]
