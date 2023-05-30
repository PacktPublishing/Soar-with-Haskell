import Prelude hiding (dropWhile)

import Data.Char (isSpace)

-- # 4.1.1 Dropping a Prefix

dropSpaces :: String -> String
dropSpaces []     = []
dropSpaces (c:cs)
  | c == ' '      = dropSpaces cs
  | otherwise     = c:cs

dropWhitespaces :: String -> String
dropWhitespaces []     = []
dropWhitespaces (c:cs)
  | isSpace c     = dropWhitespaces cs
  | otherwise     = c:cs

dropZeroes :: [Int] -> [Int]
dropZeroes []     = []
dropZeroes (c:cs)
  | c == 0        = dropZeroes cs
  | otherwise     = c:cs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []     = []
dropWhile p (c:cs)
  | p c             = dropWhile p cs
  | otherwise       = c:cs

dropSpaces' l = dropWhile isSpaceCharacter l where
  isSpaceCharacter :: Char -> Bool
  isSpaceCharacter c = c == ' '

dropWhiteSpaces' l = dropWhile isSpace l

dropZeroes' l = dropWhile isZero l where
  isZero :: Int -> Bool
  isZero n = n == 0

-- # 4.1.2 Sorting in Ascending and Descending Order

insert :: [Int] -> Int -> [Int]
insert []     y = [y]
insert (x:xs) y
  | x <= y     = x : insert xs y
  | otherwise = y : x : xs

isort :: [Int] -> [Int]
isort l = insertAll l [] where
  insertAll []     sl = sl
  insertAll (x:xs) sl = insertAll xs (insert sl x)

insertDesc :: [Int] -> Int -> [Int]
insertDesc []     y = [y]
insertDesc (x:xs) y
  | x >= y     = x : insertDesc xs y
  | otherwise = y : x : xs

isortDesc :: [Int] -> [Int]
isortDesc l = insertAll l [] where
  insertAll []     sl = sl
  insertAll (x:xs) sl = insertAll xs (insertDesc sl x)

insertGen :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insertGen comp []     y = [y]
insertGen comp (x:xs) y
  | comp x y     = x : insertGen comp xs y
  | otherwise = y : x : xs

isortGen :: (Int -> Int -> Bool) -> [Int] -> [Int]
isortGen comp l = insertAll l [] where
  insertAll []     sl = sl
  insertAll (x:xs) sl = insertAll xs (insertGen comp sl x)

isortAsc :: [Int] -> [Int]
isortAsc l = isortGen (<=) l


isortDesc' :: [Int] -> [Int]
isortDesc' l = isortGen (>=) l

-- # 4.2.1 Folding Lists

-- Predefined function:
--
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr c n []     = n
-- foldr c n (x:xs) = c x (foldr c n xs) 

sum2 :: [Integer] -> Integer
sum2 l = foldr (+) 0 l

and :: [Bool] -> Bool
and l = foldr (&&) True l

-- # 4.2.2 Folds for Other Algebraic Datatypes

-- copied from chapter 3
data Expr = Lit Int | Add Expr Expr

foldExpr :: (b -> b -> b) -> (Int -> b) -> Expr -> b
foldExpr a l (Lit n)     = l n
foldExpr a l (Add e1 e2) = a (foldExpr a l e1) (foldExpr a l e2)

eval :: Expr -> Int
eval e = foldExpr (+) id e

renderExpr :: Expr -> String
renderExpr e = foldExpr a show e where
  a :: String -> String -> String
  a s1 s2 = s1 ++ " + " ++ s2

-- copied from chapter 3
data Tree a = Empty | Branch (Tree a) a (Tree a)

foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree b e Empty          = e
foldTree b e (Branch l x r) = 
  b (foldTree b e l) x (foldTree b e r)

treeToList :: Tree a -> [a]
treeToList t = foldTree b [] t where
  b l1 x l2 = x : l1 ++ l2

-- # 4.2.3 Variations on Structural Recursion

tails' :: [a] -> [[a]]
tails' []     = []
tails' (_:xs) = xs : tails' xs

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para c n []     = n
para c n (x:xs) = c x xs (para c n xs)

tails2' :: [a] -> [[a]]
tails2' l = para c [] l where
  c _ xs r = xs : r

tails3' :: [a] -> [[a]]
tails3' l = snd (foldr c ([],[]) l) where
  c x (xs,yss) = (x:xs,xs:yss)

-- # 4.3.2 Mapping

lengthOfShow :: Int -> Int
lengthOfShow n = length (show n)

-- # 4.3.5 Folding from the Left

reverseAcc :: [a] -> [a]
reverseAcc l = go l [] where
  go :: [a] -> [a] -> [a]
  go []     acc = acc
  go (x:xs) acc = go xs (x:acc)

reverseAcc2 :: [a] -> [a]
reverseAcc2 l = foldl u [] l where
  u acc x = x:acc

sumAcc :: [Int] -> Int
sumAcc l = foldl (+) 0 l


-- # 4.4.1 Standard Deviation

average :: [Float] -> Float
average l = sum l / fromIntegral (length l)

average2 :: [Float] -> Float
average2 l = s / fromIntegral len where
  (s, len) = foldr c n l
  n = (0,0)
  c x (sr, lenr) = (sr + x, lenr + 1) 

stdev :: [Float] -> Float
stdev l = sum (map square (map diff l)) where
  m        = average l
  diff x   = x - m
  square x = x^2

-- # 4.4.2 Odds and Evens

oddsAndEvens :: [(Int,Int)] -> Bool
oddsAndEvens l = all even (map snd (filter oddFst l)) where
  oddFst :: (Int,Int) -> Bool
  oddFst (x,_) = odd x

-- # 4.4.3 All You Can Buy

data Product = MkP {name :: String, price :: Int}
  deriving Show

budgetBuy :: Int -> [Product] -> [Product]
budgetBuy budget ps = 
  map fst (takeWhile inBudget (zip ps cprices))
  where
   cprices = tail (scanl (+) 0 (map price ps))
   inBudget (p,cprice) = cprice <= budget






