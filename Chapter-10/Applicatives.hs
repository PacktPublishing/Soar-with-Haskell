import Data.Monoid
import Data.Map hiding (lookup)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Functor.Compose

-- * 10.2.1 A Family of zip Functions

zipApply :: [b -> c] -> [b] -> [c]
zipApply []     xs     = []
zipApply fs     []     = []
zipApply (f:fs) (x:xs) = f x : zipApply fs xs

-- * 10.3.1 Failing Computations with Maybe

funs :: [(String, Float -> Float)]
funs = [("sine",sin),("cosine",cos),("increment",(+1))]

params :: [(String,Float)]
params = [("pi",pi),("zero",0),("x",5)]

fun :: String -> Maybe (Float -> Float)
fun f = lookup f funs

param :: String  -> Maybe Float
param p = lookup p params

-- * 10.3.2 Erroneous Computations with Either

data ApplicationError = MissingFunction String 
                      | MissingParameter String
                      deriving Show


fun2 :: String -> Either ApplicationError (Float -> Float)
fun2 f = maybe (Left (MissingFunction f)) Right (lookup f funs)

param2 :: String  -> Either ApplicationError Float
param2 p = maybe (Left (MissingParameter p)) Right 
                 (lookup p params)

-- * 10.3.3 Implicitly Parameterized Computations with Functions

data ApplicationEnv = Env { fun3 :: Float -> Float 
                          , param3 :: Float }

prog :: ApplicationEnv -> Float
prog = fun3 <*> param3

-- * 10.3.4 Analysing Computations with the Constant Functor

fib0 :: Integer -> Integer
fib0 n
 | n < 2     = 1 
 | otherwise =  fib0 (n - 1) + fib0 (n -2)

tick :: (Sum Integer, ())
tick = (Sum 1, ())

fib :: Integer -> (Sum Integer, Integer)
fib n
 | n < 2     =  tick *> pure 1 
 | otherwise =  tick *> (pure (+) <*> fib (n - 1) <*> fib (n -2))

-- * 10.3.5 Logging Computations with Tuples

tick2 :: (Sum Integer, ())
tick2 = (Sum 1, ())

fib2 :: Integer -> (Sum Integer, Integer)
fib2 n
 | n < 2     =  tick2 *> pure 1 
 | otherwise =  tick2 *> (pure (+) <*> fib2 (n - 1) <*> fib2 (n -2))


newtype MonoidMap k v = MM { ruMM :: Map k v }
  deriving Show

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  MM m1 <> MM m2 = MM (unionWith (<>) m1 m2)

instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
  mempty = MM empty

tick3 :: Integer -> (MonoidMap Integer (Sum Integer), ())
tick3 n = (MM $ singleton n (Sum 1),())

fib3 :: Integer -> (MonoidMap Integer (Sum Integer), Integer)
fib3 n
 | n < 2     =  tick3 n *> pure 1 
 | otherwise =  tick3 n *> (pure (+) <*> fib3 (n - 1) <*> fib3 (n -2))

-- * 10.3.6 Stateful Computations with State

fib4 :: Integer -> (State (Map Integer Integer) Integer)
fib4 n = state (\ s -> case Map.lookup n s of
                         Just f  -> (f, s)
                         Nothing -> let (f, s') = runState (fib4' n) s
                                    in  (f, insert n f s'))
  where
    fib4' n
      | n < 2     = pure 1
      | otherwise = (+) <$> fib4 (n-1) <*> fib4 (n-2)

-- * 10.3.7 Nondeterministic Computations with Lists

basePrice :: [Float]
basePrice = [1000,3500]

vat :: [Float]
vat = [0.06,0.21]

-- * 10.3.9 Compositions

type Memoization = State (Map Integer Integer)
type Logging     = (,) (MonoidMap Integer (Sum Integer)) 
type Effect      = Compose Memoization Logging

fib5 :: Integer -> Effect Integer
fib5 n = Compose (state (\ s -> case Map.lookup n s of
                         Just f  -> (pure f, s)
                         Nothing -> let ((m,f), s') = runState (getCompose (fib5' n)) s
                                    in  ((m,f), insert n f s'))) where
   fib5' n
     | n < 2     = lift2 (tick3 n) *> pure 1
     | otherwise = lift2 (tick3 n) *> ((+) <$> fib5 (n-1) <*> fib5 (n-2))

   lift2 q = Compose (pure q)
