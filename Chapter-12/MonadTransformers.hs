{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (log)
import Control.Monad

data LogLevel = Mundane | Important | Critical
  deriving (Eq, Ord, Show)

mundane, important, critical :: String -> App ()
mundane   = log Mundane
important = log Important
critical  = log Critical

data Config = MkConfig { shouldLog :: LogLevel -> Bool }

cfgAll, cfgNone, cfgCritical :: Config
cfgAll      = MkConfig (\_ -> True)
cfgNone     = MkConfig (\_ -> False)
cfgCritical = MkConfig (== Critical)

type Log = [(LogLevel, String)]

fac :: Integer -> App Integer
fac n =
  do mundane ("Calling: fac " ++ show n)
     r <- go n
     important ("Returning: fac " ++ show n ++ " = " ++ show r)
     pure r
  where
    go n
     | n < 0     = do critical ("Negative input: " ++ show n)
                      pure 1
     | n == 0    = pure 1
     | otherwise = do r <- fac (n-1)
                      pure (n * r)

newtype App a = MkApp { unApp :: Config -> (a, Log) }
  deriving Functor

log :: LogLevel -> String -> App ()
log lvl msg = MkApp (\cfg ->
  if shouldLog cfg lvl 
    then ((), [(lvl,msg)])
    else ((), []))

runApp :: Config -> App a -> (a, Log)
runApp cfg app = unApp app cfg

instance Monad App where
  p >>= f = MkApp (\cfg -> 
    let (x, log1) = runApp cfg p
        (y, log2) = runApp cfg (f x)
    in (y, log1 ++ log2))

instance Applicative App where
  pure x = MkApp (\cfg -> (x, []))
  (<*>) = ap


