import Prelude hiding (log)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Char
import Data.Map hiding (lookup,filter,null,map)
import qualified Data.Map as Map


-- * Failing with Maybe

funs :: [(String, Float -> Float)]
funs = [("sine",sin),("cosine",cos),("increment",(+1))]

params :: [(String,Float)]
params = [("pi",pi),("zero",0),("x",5)]

fun :: String -> Maybe (Float -> Float)
fun f = lookup f funs

param :: String  -> Maybe Float
param p = lookup p params

prog :: Maybe Float
prog = fun "inc" <*> param "one"

dictionary :: [(String,String)]
dictionary = [("sinus","sine")
             ,("cosinus","cosine")
             ,("verhoog","increment")
             ,("verlaag","decrement")]

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing  f = Nothing
andThen (Just x) f = f x

dutchFun :: String -> Maybe (Float -> Float)
dutchFun dutchName = 
  lookup dutchName dictionary `andThen` \englishName ->
  lookup englishName funs

germanFun :: String -> Maybe (Float -> Float)
germanFun germanName = 
  lookup germanName germanDutchDictionary `andThen` \dutchName ->
  lookup dutchName dictionary `andThen` \englishName ->
  lookup englishName funs

germanDutchDictionary :: [(String,String)]
germanDutchDictionary =
  [("Sinus","sinus")
  ,("erhöhe","verhoog)")]

-- * State Passing

andNext :: State s a -> (a -> State s b) -> State s b
andNext p k = state (\s0 -> let (x,s1) = runState p s0                           
                            in runState (k x) s1)

fib :: Integer -> State (Map Integer Integer) Integer
fib n = 
  get `andNext` \ s -> 
    case Map.lookup n s of
      Just f  -> pure f
      Nothing -> fib' n              `andNext` \f ->
                 modify (insert n f) `andNext` \_ ->
                 pure f
  where
    fib' n
      | n < 2     = pure 1
      | otherwise = (+) <$> fib (n-1) <*> fib (n-2)

-- * The Monad Type Class

germanFun2 :: String -> Maybe (Float -> Float)
germanFun2 germanName =
  do dutchName   <- lookup germanName germanDutchDictionary
     englishName <- lookup dutchName dictionary
     lookup englishName funs

fib2 :: Integer -> State (Map Integer Integer) Integer
fib2 n = 
  do s <- get
     case Map.lookup n s of
       Just f  -> pure f
       Nothing -> do f <- fib' n
                     modify (insert n f)
                     pure f
  where
    fib' n
      | n < 2     = pure 1
      | otherwise = (+) <$> fib2 (n-1) <*> fib2 (n-2)


-- * The Error (aka Exception) Monad

data EmptyList = EmptyList

percentage :: (a -> Bool) -> [a] -> Either EmptyList Float
percentage p l
  | null l    = throwError EmptyList
  | otherwise = pure (  fromIntegral (length (filter p l)) 
                     / fromIntegral (length l))

data Toss = Heads | Tails deriving Eq

guessHeads :: [Toss] -> Either EmptyList Float
guessHeads tosses = catchError (percentage (== Heads) tosses)
                               (\EmptyList -> pure 0.5)

guessHeads' :: [Toss] -> Float
guessHeads' tosses = either (\EmptyList -> 0.5) id 
                      (percentage (==Heads) tosses)

-- * The Reader (aka Environment) Monad

price :: Int -> Reader Float Float
price qty = 
  do vat <- ask
     pure (57.0 * (fromIntegral qty) * (1 + vat))

data AppConfiguration = 
  MkAppConfiguration {
    verbosityLevel :: Int,
    userName :: String
  }

warn :: String -> Reader AppConfiguration String
warn msg = do vl <- asks verbosityLevel
              if vl > 2 then pure (map toUpper msg)
                        else pure msg

localExample :: Reader Int (Int,Int,Int)
localExample = do x <- ask
                  y <- local (*2) ask
                  z <- ask
                  pure (x,y,z)

data Rose a = Node a [Rose a] 

instance Show a => Show (Rose a) where
  show n = unlines $ runReader (goN n) 0

goN :: Show a => Rose a -> Reader Int [String]
goN (Node x xs) =
  (:) <$> indent ("- " ++ show x) <*> local (+2) (goL xs)

goL :: Show a => [Rose a] -> Reader Int [String]
goL l = concat <$> traverse goN l

indent :: String -> Reader Int String
indent s = do n <- ask
              pure (replicate n ' ' ++ s)

example = Node 1 [Node 2
                   [Node 4
                     [Node 7 [] 
                     ,Node 8 [] 
                     ,Node 9 []]
                   ,Node 5 []]
                 ,Node 3
                   [Node 6 []]]

log :: String -> Writer [String] ()
log msg = tell [msg]

fac :: Integer -> Writer [String] Integer
fac n =
  do log ("Calling: fac " ++ show n)
     r <- go n
     log ("Returning: fac " ++ show n ++ " = " ++ show r)
     pure r
  where
    go n
     | n <= 0    = pure 1
     | otherwise = do r <- fac (n-1)
                      pure (n * r)

displayLog :: Writer [String] a -> IO ()
displayLog p = let (_, l) = runWriter p
               in mapM_ putStrLn l

-- * The List (aka Nondeterminism) Monad

boxes :: [(Int,Int)]
boxes = do large <- [1..5]
           small <- [1..(large-1)]
           pure (large,small)

boxes' :: [(Int,Int)]
boxes' = [ (large, small)
         | large <- [1..5]
         , small <- [1..(large-1)]
         ]

boxes2' :: [(Int,Int)]
boxes2' = [ (large, small)
          | large <- [1..5]
          , small <- [1..5]
          , small < large
          ]

boxes2 :: [(Int,Int)]
boxes2 = do large <- [1..5]
            small <- [1..5]
            guard (small < large)
            pure (large,small)


