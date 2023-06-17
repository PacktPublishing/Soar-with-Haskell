{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')

-- * 7.1.4 Call by Value

loop :: a -> a
loop x = loop x

-- * 7.2.3 Bottom

process :: String -> Float
process = step3 . step2 . step1

step1 :: String -> Int
step1 = undefined

step2 :: Int -> Int
step2 = undefined

step3 :: Int -> Float
step3 = undefined

-- * 7.2.4 Structured Data

data D = MkD Bool
newtype N = MkN Bool

-- * 7.3.3 Corecursive Programs

ones :: [Integer]
ones = 1 : ones

-- * 7.4.1 The Leaking Accumulator 

sumAcc :: [Integer] -> Integer
sumAcc l = go l 0 where
  go :: [Integer] -> Integer -> Integer
  go []     acc = acc
  go (x:xs) acc = go xs (acc + x)

sumAcc' :: [Integer] -> Integer
sumAcc' l = foldl (+) 0 l

-- * 7.4.2 Strictness Annotations

sumAcc2 :: [Integer] -> Integer
sumAcc2 l = go l 0 where
  go :: [Integer] -> Integer -> Integer
  go []     acc = acc
  go (x:xs) acc = let acc' = acc + x
                  in  acc' `seq` go xs acc'

sumAcc2' :: [Integer] -> Integer
sumAcc2' l = foldl' (+) 0 l

sumAcc3 :: [Integer] -> Integer
sumAcc3 l = go l 0 where
  go :: [Integer] -> Integer -> Integer
  go []      acc = acc
  go (x:xs) !acc = go xs (acc + x)

data Point = MkPoint !Int !Int

-- * 7.4.3 Increased Laziness

splitList1 :: [a] -> ([a],[a])
splitList1 l = foldr (\x (ys,zs) -> (x:zs, ys)) ([],[]) l

splitList2 :: [a] -> ([a],[a])
splitList2 l = foldr c ([],[]) l
  where
    c x r = (x : fst r, snd r)

splitList3 :: [a] -> ([a],[a])
splitList3 l = foldr c ([],[]) l
  where
    c x ~(ys,zs) = (x:zs, ys)

splitList4 :: [a] -> ([a],[a])
splitList4 []     = ([],[])
splitList4 (x:xs) = (x:zs,ys)
   where (ys,zs) = splitList3 xs
