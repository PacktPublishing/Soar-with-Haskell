import Prelude hiding (sum, product, and, reverse, odd, even, zip, concat)

import Data.List (tails)

-- 3.1.1 List Syntax

seasons :: [String]
seasons = ["spring","summer","fall","winter"]

-- copied from Chapter 2
data Suit = Clubs | Spades | Diamonds | Hearts
  deriving Show

redSuits :: [Suit]
redSuits = [Hearts,Diamonds]

noBools :: [Bool]
noBools = []

anInt :: [Int]
anInt = [7]

-- # 3.1.2 List Syntax Desugared

redSuits2 :: [Suit]
redSuits2 = Hearts : (Diamonds : [])

redSuits3 :: [Suit]
redSuits3 = Hearts : Diamonds : []

addOne :: [Int] -> [Int]
addOne l = 1 : l

-- # 3.1.3 Predefined List Functions

sum :: [Integer] -> Integer
sum l
  | null l     = 0
  | otherwise  = head l + sum (tail l)

-- # 3.1.4 List Comprehensions

triples :: [(Int,Int,Int)]
triples = [ (a,b,c) | a <- [1..100]
                    , b <- [1..100]
                    , c <- [1..100]
                    , a^2 + b^2 == c^2
                    ]

triples2 :: [(Int,Int,Int)]
triples2 = [ (a,b,c) | a <- [1..100]
                     , b <- [1..100]
                     , c <- [1..100]
                     , a^2 + b^2 == c^2
                     , a < b
                     ]

triples3 :: [(Int,Int,Int)]
triples3 = [ (a,b,c) | a <- [1..100]
                     , b <- [1..100]
                     , a < b
                     , c <- [1..100]
                     , a^2 + b^2 == c^2
                     ]

triples4 :: [(Int,Int,Int)]
triples4 = [ (a,b,c) | a <- [1..100]
                     , b <- [(a+1)..100]
                     , c <- [1..100]
                     , a^2 + b^2 == c^2
                     ]

triples5 :: [(Int,Int,Int)]
triples5 = [ (a,b,c) | a <- [1..100]
                     , b <- [(a+1)..100]
                     , c <- [(b+1)..100]
                     , a^2 + b^2 == c^2
                     ]

-- # 3.2 Custom List Processing

evenLength :: [a] -> Bool
evenLength []     = True
evenLength (x:xs) = not (evenLength xs)

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- # 3.3.1 Arithmetic Expressions

data Expr = Lit Int | Add Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2

renderExpr :: Expr -> String
renderExpr (Lit n)     = show n
renderExpr (Add e1 e2) = renderExpr e1 ++ " + " ++ renderExpr e2

-- # 3.3.3 Parametric Recursive Datatypes

data List a = Nil | Cons a (List a)

lengthList :: List a -> Int
lengthList Nil         = 0
lengthList (Cons x xs) = 1 + lengthList xs

data Tree a = Empty | Branch (Tree a) a (Tree a)

tree123 :: Tree Int
tree123 = Branch (Branch Empty 2 Empty)
                 1
                 (Branch Empty 3 Empty)

treeToList :: Tree a -> [a]
treeToList Empty          = []
treeToList (Branch l x r) = x : treeToList l ++ treeToList r

-- # 3.4.1 Structural Recursion on Lists

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

-- # 3.4.2 Structural Recursion on Other Algebraic Datatypes

literals :: Expr -> [Int]
literals (Lit n)     = [n]
literals (Add e1 e2) = literals e1 ++ literals e2

-- # 3.5.1 Primitive Recursion

product :: [Integer] -> Integer
product []     = 1
product (x:xs) = x * product xs

tails' :: [a] -> [[a]]
tails' []     = []
tails' (_:xs) = xs : tails' xs

-- Predefined in Data.List
-- tails :: [a] -> [[a]]
-- tails l = l : tails' l

allPairs :: [a] -> [(a,a)]
allPairs l = [ (x,y) | (x:xs) <- tails l, y <- xs ]

-- # 3.5.2 Recursion on Integers

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

fac' :: Integer -> Integer
fac' n
  | n <= 0    = 1
  | otherwise = n * fac' (n - 1)

-- # 3.5.3 Additional Parameters

-- Predefined
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

data Expr2 = X2 | Lit2 Int | Add2 Expr2 Expr2

eval2 :: Expr2 -> Int -> Int
eval2 X2           x = x
eval2 (Lit2 n)     x = n
eval2 (Add2 e1 e2) x = eval2 e1 x + eval2 e2 x

-- # 3.5.4 Varying Parameters and the Worker/Wrapper Structure

data Expr3 = X3 | Lit3 Int | Add3 Expr3 Expr3 | Mul3 Expr3 Expr3

renderExpr2 :: Expr3 -> String
renderExpr2 X3           = "x"
renderExpr2 (Lit3 n)     = show n
renderExpr2 (Add3 e1 e2) = renderExpr2 e1 ++ " + " ++ renderExpr2 e2
renderExpr2 (Mul3 e1 e2) = renderExpr2 e1 ++ " * " ++ renderExpr2 e2 

renderExpr3 :: Expr3 -> String
renderExpr3 X3           = "x"
renderExpr3 (Lit3 n)     = show n
renderExpr3 (Add3 e1 e2) = 
  parens (renderExpr3 e1 ++ " + " ++ renderExpr3 e2)
renderExpr3 (Mul3 e1 e2) = renderExpr3 e1 ++ " * " ++ renderExpr3 e2 

parens :: String -> String
parens s = "(" ++ s ++ ")"

type Prec = Int

basePrec, addPrec, mulPrec :: Prec
basePrec = 0
addPrec  = 6
mulPrec  = 7

renderExpr4 :: Expr3 -> String
renderExpr4 e = go e basePrec where

  go :: Expr3 -> Prec -> String
  go X3           p = "x"
  go (Lit3 n)     p = show n
  go (Add3 e1 e2) p = 
    parensP p addPrec (go e1 addPrec ++ " + " ++ go e2 addPrec)
  go (Mul3 e1 e2) p = 
    go e1 mulPrec ++ " * " ++ go e2 mulPrec

  parensP :: Prec -> Prec -> String -> String
  parensP p1 p2 s
    | p1 <= p2  = s
    | otherwise = parens s

-- # 3.5.5 Accumulation

sumAcc :: [Integer] -> Integer
sumAcc l = go l 0 where
  go :: [Integer] -> Integer -> Integer
  go []     acc = acc
  go (x:xs) acc = go xs (acc + x)

reverseAcc :: [a] -> [a]
reverseAcc l = go l [] where
  go :: [a] -> [a] -> [a]
  go []     acc = acc
  go (x:xs) acc = go xs (x:acc)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- # 3.5.6 Recursion on Nested Datatypes

doubleTree :: Tree [Integer] -> Tree [Integer]
doubleTree Empty 
  = Empty
doubleTree (Branch l xs r)
  = Branch (doubleTree l) (doubleList xs) (doubleTree r)

doubleList :: [Integer] -> [Integer]
doubleList []     = []
doubleList (x:xs) = (2*x) : doubleList xs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

concat' :: [[a]] -> [a]
concat' xss = [ x | xs <- xss, x <- xs ]

-- # 3.5.7 Mutual Recursion

even, odd :: Integer -> Bool
even 0 = True
even n = odd (n-1)

odd 0 = False
odd n = even (n-1)

data RoseTree a = Node a (Forest a)
data Forest a = Nil' | Cons' (RoseTree a) (Forest a)

sumR :: RoseTree Int -> Int
sumR (Node x f) = x + sumF f

sumF :: Forest Int -> Int
sumF Nil'        = 0
sumF (Cons' r f) = sumR r + sumF f

concat3 :: [[a]] -> [a]
concat3 []       = []
concat3 (xs:xss) = concatAux xs xss where
  concatAux :: [a] -> [[a]] -> [a]
  concatAux []     xss = concat3 xss
  concatAux (x:xs) xss = x : concatAux xs xss

-- # 3.5.8 Simultaneous Recursion on Multiple Structures

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = [] 
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- # 3.5.9 Combining Variations

insert :: [Int] -> Int -> [Int]
insert []     y = [y]
insert (x:xs) y
  | x <= y     = x : insert xs y
  | otherwise = y : x : xs

-- # 3.6.1 Non-Termination

loop :: Integer -> Integer
loop x = loop x

diverge :: [Integer] -> Integer
diverge xs = diverge (1:xs)

-- # 3.6.2 Unbounded Search

nextPerfect :: Integer -> Integer
nextPerfect n
  | perfect n = n
  | otherwise = nextPerfect (n+1)

perfect :: Integer -> Bool
perfect n = sum (divisors n) == n

divisors :: Integer -> [Integer] 
divisors n = [ d | d <- [1..n-1], mod n d == 0 ]

collatz :: Integer -> [Integer]
collatz n
  | n <= 1    = n : []
  | even n    = n : collatz (div n 2)
  | otherwise = n : collatz (3 * n + 1)







