{-# LANGUAGE DeriveFoldable #-}

import Control.Parallel (par, pseq)
import Data.Map (Map)
import qualified Data.List as List
import Data.Foldable

-- * 9.1.2 Monoids

divideAndConquer :: Monoid m => [m] -> m
divideAndConquer ms = go ms (length ms) where
  go []  n = mempty
  go [m] n = m
  go ms  n = go ms1 n1 <> go ms2 n2
    where n1 = div n 2
          n2 = n - n1
          (ms1,ms2) = splitAt n1 ms

parallelDivideAndConquer :: Monoid m => [m] -> m
parallelDivideAndConquer ms = go ms (length ms) where
  go []  n = mempty
  go [m] n = m
  go ms  n = pseq (par x y) (x <> y) 
    where n1 = div n 2
          n2 = n - n1
          (ms1,ms2) = splitAt n1 ms
          x = go ms1 n1
          y = go ms2 n2


-- * 9.2.3 Type Constructor Parameters

data Rose a = RNode a [Rose a]

data Trie k a = TNode a (Map k (Trie k a))

data GRose c a = GNode a (c (GRose c a))


type Rose' = GRose []
type Trie' k = GRose (Map k)
type NonEmptyList = GRose Maybe

nonEmptyToList :: NonEmptyList a -> [a]
nonEmptyToList (GNode x xs) = x : go xs where
  go :: Maybe (NonEmptyList a) -> [a]
  go Nothing  = []
  go (Just l) = nonEmptyToList l

-- * 9.3.2 Listable Collections

data Tree1 a = Leaf1 a | Fork1 (Tree1 a) (Tree1 a)

treeToList :: Tree1 a -> [a]
treeToList (Leaf1 x)   = [x]
treeToList (Fork1 l r) = treeToList l ++ treeToList r

instance Foldable Tree1 where
    toList    = treeToList
    sum       = List.sum . treeToList
    product   = List.product . treeToList
    foldr c n = List.foldr c n . treeToList

data Tree2 a = Leaf2 a | Fork2 (Tree2 a) (Tree2 a)

instance Foldable Tree2 where
    foldr c n (Leaf2 x)   = c x n
    foldr c n (Fork2 l r) = foldr c (foldr c n r) l

data Tree3 a = Leaf3 a | Fork3 (Tree3 a) (Tree3 a)

instance Foldable Tree3 where
    foldMap f (Leaf3 x)   = f x
    foldMap f (Fork3 l r) = foldMap f l <> foldMap f r

instance Foldable Rose where
    foldMap f (RNode x xs) = f x <> foldMap (foldMap f) xs

instance Foldable c => Foldable (GRose c) where
    foldMap f (GNode x xs) = f x <> foldMap (foldMap f) xs

data Tree4 a = Leaf4 a | Fork4 (Tree4 a) (Tree4 a)
    deriving Foldable


-- * 9.4.1 Sorted Lists

sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [x]      = True
sorted (x:y:zs) = x <= y && sorted (y:zs)

-- * 9.4.3 A Richer Approach with Structural Recursion

data Sortedness1 a = NotSorted1 | Sorted1 a | EmptySorted1

sorted' :: Ord a => [a] -> Sortedness1 a
sorted' l = foldr c n l where

  n :: Sortedness1 a
  n = EmptySorted1

  c :: Ord a => a -> Sortedness1 a -> Sortedness1 a
  c x NotSorted1   = NotSorted1
  c x EmptySorted1 = Sorted1 x
  c x (Sorted1 y)
    | x <= y       = Sorted1 x
    | otherwise    = NotSorted1

sorted2 :: Ord a => [a] -> Bool
sorted2 l = sortednessToBool1 (sorted' l)

sortednessToBool1 :: Sortedness1 a -> Bool
sortednessToBool1 NotSorted1   = False
sortednessToBool1 (Sorted1 _)  = True
sortednessToBool1 EmptySorted1 = True

-- * 9.4.4 The Sortedness Monoid

data Sortedness a = NotSorted | Sorted a a | EmptySorted

instance Ord a => Semigroup (Sortedness a) where
  EmptySorted <> s = s
  s <> EmptySorted = s
  NotSorted <> s = NotSorted
  s <> NotSorted = NotSorted
  Sorted l1 u1 <> Sorted l2 u2
    | u1 <= l2   = Sorted l1 u2
    | otherwise  = NotSorted

instance Ord a => Monoid (Sortedness a) where
  mempty = EmptySorted

sorted3 :: (Foldable t, Ord a) => t a -> Bool
sorted3 l = sortednessToBool (foldMap (\x -> Sorted x x) l)

sortednessToBool :: Sortedness a -> Bool
sortednessToBool NotSorted   = False
sortednessToBool (Sorted _ _)  = True
sortednessToBool EmptySorted = True
