import Data.List (nub)
import Data.Map hiding (foldr, map, filter, size)

-- * 6.1.1 What is Ad-Hoc Polymorphism

data Suit = Hearts | Diamonds | Clubs | Spades
  -- deriving (Eq, Ord)

-- * 6.1.3 Type Class Constraints Are Contagious

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x []     = []
deleteAll x (y:ys)
  | x == y         = deleteAll x ys
  | otherwise      = y : deleteAll x ys


deleteAll' :: Eq a => a -> [a] -> [a]
deleteAll' x ys = filter (/= x) ys

example :: [Integer]
example = nub [1,2,3,1]

-- * 6.2.1 Type Class Instantiation

instance Eq Suit where

  Hearts   == Hearts   = True
  Diamonds == Diamonds = True
  Spades   == Spades   = True
  Clubs    == Clubs    = True
  _        == _        = False

  -- Hearts   /= Hearts   = False
  -- Diamonds /= Diamonds = False
  -- Spades   /= Spades   = False
  -- Clubs    /= Clubs    = False
  -- _        /= _        = True

-- * 6.2.3 Instances for Composite Values

data Point = MkPoint Int Int

instance Eq Point where
  MkPoint x1 y1 == MkPoint x2 y2 = x1 == x2 && y1 == y2

-- ** First variant of Expr

data Expr1 = Lit1 Int | Add1 Expr1 Expr1

instance Eq Expr1 where
  Lit1 n     == Lit1 m     = n == m
  Add1 e1 e2 == Add1 e3 e4 = e1 == e3 && e3 == e4
  _          == _          = False

-- ** Second variant of Expr

data Expr2 = Lit2 Int | Add2 Expr2 Expr2

eqExpr :: Expr2 -> Expr2 -> Bool
eqExpr (Lit2 n)     (Lit2 m)     = n == m
eqExpr (Add2 e1 e2) (Add2 e3 e4) = eqExpr e2 e2 && eqExpr e3 e4
eqExpr _            _            = False

instance Eq Expr2 where
  (==) = eqExpr

-- * 6.2.4 Deriving Structural Instances

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving Eq

-- * 6.2.3 Semantic Equality Instances

-- ** Third variant of Expr

data Expr3 = Lit3 Int | Add3 Expr3 Expr3

eval :: Expr3 -> Int
eval (Lit3 n)     = n
eval (Add3 e1 e2) = eval e1 + eval e2

instance Eq Expr3 where
  e1 == e2 = eval e1 == eval e2

-- * 6.2.4 Lawful Instances

size :: Expr3 -> Int
size (Lit3 n) = 1
size (Add3 e1 e2) = 1 + size e1 + size e2

-- * 6.3.1 Ord

instance Ord Suit where
  Diamonds <= _      = True
  Clubs    <= Clubs  = True
  Clubs    <= Hearts = True
  Hearts   <= Hearts = True
  _        <= Spades = True
  _        <= _      = False


-- * 6.3.4 Bounded and Enum 

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
  deriving (Show, Enum)

-- * 6.3.5 Numeric Type Classes

data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | Negate Expr | Abs Expr | Sign Expr | Var String 
  deriving Show

instance Num Expr where
  (+) = Add
  (*) = Mul
  negate = Negate
  abs = Abs
  signum = Sign
  fromInteger = Lit 

anExpression :: Num a => a -> a -> a
anExpression x y = 2 * x - y

-- * 6.4.2 The Map Library

frequency :: Ord a => [a] -> Map a Int
frequency []     = empty
frequency (x:xs) = insertWith (+) x 1 (frequency xs)

-- * 6.5.1 Text Boxes

data Box = MkBox { content :: [String], width :: Int, height :: Int }

exampleBox :: Box
exampleBox = MkBox ["abcd","efgh","ijkl"] 4 3

emptyBox :: Box
emptyBox = MkBox [] 0 0

instance Show Box where
  show = unlines . content

-- * 6.5.2 Box Combinators

frame :: Box -> Box
frame (MkBox c w h) = MkBox ([tline] ++ map vline c ++ [bline]) (w+2) (h+2)
  where
   tline = "┌" ++ replicate w '─' ++ "┐"
   bline = "└" ++ replicate w '─' ++ "┘"
   vline l = "│" ++ l ++ "│"

vcomp :: Box -> Box -> Box
vcomp (MkBox c1 w1 h1) (MkBox c2 w2 h2)
  = MkBox (c1' ++ c2') w h
  where
    w = max w1 w2 
    h = h1 + h2
    pad n l = l ++ replicate n ' '
    c1' = map (pad (w - w1)) c1
    c2' = map (pad (w - w2)) c2

vcompList :: [Box] -> Box
vcompList bs = foldr vcomp emptyBox bs

hcomp :: String -> Box -> Box -> Box
hcomp sep (MkBox c1 w1 h1) (MkBox c2 w2 h2)
  = MkBox (zipWith (\l1 l2 -> l1 ++ sep ++ l2) c1' c2') w h
  where
    w = w1 + length sep + w2 
    h = max h1 h2
    pad n m l = l ++ replicate n (replicate m ' ')
    c1' = pad (h - h1) w1 c1
    c2' = pad (h - h2) w2 c2

-- * 6.5.2 Boxable Things

class Boxable a where
  toBox :: a -> Box

instance Boxable Char where
  toBox c = MkBox [[c]] 1 1

instance Boxable Int where
  toBox n = MkBox [s] n 1
    where s = replicate n '◼'

instance (Boxable k, Boxable v) => Boxable (Map k v) where
  toBox m = frame (hcomp "│" col1 col2)
    where (ks, vs) = unzip (toList m)
          col1 = vcompList (map toBox ks)
          col2 = vcompList (map toBox vs)

frequencyBox :: (Ord a, Boxable a) => [a] -> Box
frequencyBox = toBox . frequency

instance Boxable Suit where
  toBox Hearts   = toBox '♥'
  toBox Diamonds = toBox '♦'
  toBox Clubs    = toBox '♣'
  toBox Spades   = toBox '♠'
