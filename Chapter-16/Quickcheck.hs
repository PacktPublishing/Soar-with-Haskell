import Test.QuickCheck

horner :: [Int] -> Int -> Int
horner coeffs x = foldl (\r c -> c + x * r) 0 coeffs

prop_sum_at_one :: [Int] -> Bool
prop_sum_at_one cs = horner cs 1 == sum cs

prop_at_zero :: Int -> [Int] -> Bool
prop_at_zero c cs = horner (c:cs) 0 == c

data Button = On | Off deriving (Show, Eq)

toggle:: Button -> Button
toggle On  = Off
toggle Off = On

prop_toggle b = toggle (toggle b) == b 

instance Arbitrary Button where
  arbitrary = elements [On,Off]

data Tree = Leaf Int | Fork Tree Tree deriving (Show, Eq)


toList :: Tree -> [Int]
toList (Leaf x) = [x]
toList (Fork l r) = toList l ++ toList r

mirror :: Tree -> Tree
mirror (Leaf x) = Leaf x
mirror (Fork l r) = Fork (mirror r) (mirror l)

prop_mirror_toList :: Tree -> Property
prop_mirror_toList t = 
 label ("tree height is " ++ show (height t))
--  label ("tree size is " ++ show (size t))
    (toList (mirror t) == reverse (toList t))

height :: Tree -> Int
height (Leaf x)   = 0
height (Fork l r) = 1 + max (height l) (height r)

size :: Tree -> Int
size (Leaf x)   = 1
size (Fork l r) = 1 + size l + size r

instance Arbitrary Tree where
  -- arbitrary = oneof [leaf, fork]
  -- arbitrary = frequency [(2,leaf), (3,fork)]
{-
  arbitrary = 
    sized (\s ->
      if s > 0 
        then
          resize (s-1) (
            frequency [(1,leaf),(9,fork)]
        )
        else 
          leaf
    )
-}
  arbitrary = 
    sized (\s ->
      if s > 0 
        then
          resize (s `div` 2) (
            frequency [(1,leaf),(9,fork)]
        )
        else 
          leaf
    )
    where leaf = Leaf <$> arbitrary
          fork = Fork <$> arbitrary <*> arbitrary
  shrink (Leaf x)    = [Leaf y | y <- shrink x]
  shrink (Fork l r)  = l : r : [Fork l' r | l' <- shrink l] 
                            ++ [Fork l r' | r' <- shrink r]


prop_abs_pos :: Int -> Property
prop_abs_pos n = n >= 0 ==> abs n == n

fibSlow :: Integer -> Integer
fibSlow 0 = 1
fibSlow 1 = 1
fibSlow n = fibSlow (n-1) + fibSlow (n-2)

fibFast :: Integer -> Integer
fibFast n = go n 0 1 where
  go 0 _ r = r
  go n a b = go (n-1) b (a+b)

prop_fib :: Integer -> Property
prop_fib n = label (show n) (n >= 0 ==> fibFast n == fibSlow n)

positive :: Gen Integer
positive = fmap abs arbitrary

-- mirror :: Tree -> Tree
-- mirror (Leaf x) = Leaf x
-- mirror (Fork l r) = Fork (mirror l) (mirror r)
