
-- * 5.1.1 Anonymous Function Syntax

dropSpaces :: [Char] -> [Char]
dropSpaces l = dropWhile (\c -> c == ' ') l

-- * 5.3.2 Eta Reduction with Partial Application

squareList1 :: [Int] -> [Int]
squareList1 l = map (\x -> x * x) l

squareList2 :: [Int] -> [Int]
squareList2 = \l -> map (\x -> x * x) l

squareList3 :: [Int] -> [Int]
squareList3 = map (\x -> x * x)

-- * 5.3.4 Irreducible Cases

doubleApply :: (a -> a -> b) -> (a -> b)
doubleApply f x = f x x

-- * 5.5.1 Evaluation with Many Variables

data Expr = Var String | Lit Int | Add Expr Expr

expr :: Expr
expr = Add (Var "x") (Add (Var "y") (Lit 3))

-- two copies for the two representations of the environment
eval1 :: Expr -> Env1 -> Int
eval1 (Var v) env = lookupEnv1 env v
eval1 (Lit n) env = n
eval1 (Add e1 e2) env = eval1 e1 env + eval1 e2 env

eval2 :: Expr -> Env2 -> Int
eval2 (Var v) env = lookupEnv2 env v
eval2 (Lit n) env = n
eval2 (Add e1 e2) env = eval2 e1 env + eval2 e2 env

-- * 5.5.2 Association List

type Env1 = [(String,Int)]

lookupEnv1 :: Env1 -> String -> Int
lookupEnv1 [] _ = 0
lookupEnv1 ((v,n):env) w
  | v == w     = n
  | otherwise  = lookupEnv1 env w

-- * 5.5.3 Functional Environments

type Env2 = String -> Int

lookupEnv2 :: Env2 -> String -> Int
lookupEnv2 env = env

nil :: Env2
nil = \ w -> 0


infixr 5 +:

(+:) :: (String,Int) -> Env2 -> Env2
(v,n) +: env = \w -> if v == w then n else env w

