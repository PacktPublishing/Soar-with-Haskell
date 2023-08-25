{-# LANGUAGE DeriveFunctor #-}

data Pair a = MkPair a a
  deriving Show

instance Functor Pair where
  fmap f (MkPair x y) = MkPair (f x) (f y)

data Pair2 a = MkPair2 a a
  deriving (Functor, Show)
