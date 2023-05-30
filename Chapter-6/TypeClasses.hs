data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
  deriving (Show, Enum)

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
