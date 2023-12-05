{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.Maybe (isJust)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
  deriving Show

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprSize :: Expr -> Int
exprSize (Lit _) = 1
exprSize (Add e1 e2) = 1 + exprSize e1 + exprSize e2
exprSize (Sub e1 e2) = 1 + exprSize e1 + exprSize e2
exprSize (Mul e1 e2) = 1 + exprSize e1 + exprSize e2

data Instr = Push Int | Plus | Minus | Times
type Prog = [Instr]

type Stack = [Int]

exec :: Prog -> Stack -> Maybe Stack 
exec []     s = Just s
exec (i:is) s = instr i s >>= exec is 

instr :: Instr -> Stack -> Maybe Stack
instr (Push n) stack           = Just (n : stack)
instr Plus     (x : y : stack) = Just (x + y : stack)
instr Minus    (x : y : stack) = Just (x - y : stack)
instr Times    (x : y : stack) = Just (x * y : stack)
instr _        _               = Nothing

compile :: Expr -> Prog
compile (Lit n)     = [Push n]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [Plus]
-- compile (Sub e1 e2) = compile e1 ++ compile e2 ++ [Minus]
compile (Sub e1 e2) = compile e2 ++ compile e1 ++ [Minus]
compile (Mul e1 e2) = compile e1 ++ compile e2 ++ [Times]

prop_same_size :: Expr -> Bool
prop_same_size e =
  exprSize e == length (compile e)

prop_exec_compile_succeeds :: Expr -> Bool
prop_exec_compile_succeeds e =
  isJust (exec (compile e) [])

prop_stack_discipline :: Expr -> Stack -> Bool
prop_stack_discipline e s =
  fmap tail (exec (compile e) s) == Just s

prop_same_result :: Expr -> Bool
prop_same_result e =
  exec (compile e) [] == Just [eval e]

instance Arbitrary Expr where
  arbitrary = sized genExpr
  shrink (Lit n) = Lit <$> shrink n
  shrink (Add e1 e2) = [e1, e2] ++ [Add e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (Sub e1 e2) = [e1, e2] ++ [Sub e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (Mul e1 e2) = [e1, e2] ++ [Mul e1' e2' | (e1', e2') <- shrink (e1, e2)]

genExpr :: Int -> Gen Expr
genExpr 0 = Lit <$> arbitrary
genExpr n = oneof
  [ Lit <$> arbitrary
  , Add <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)
  , Sub <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)
  , Mul <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)
  ]

return []
runTests = $quickCheckAll
