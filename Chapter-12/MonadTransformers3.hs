{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

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

type App = ReaderT Config (Writer Log)

log :: LogLevel -> String -> App ()
log lvl msg = 
  do p <- asks shouldLog
     when (p lvl) 
       (lift (tell [(lvl,msg)]))

runApp :: Config -> App a -> (a, Log)
runApp cfg app = runWriter (runReaderT app cfg)
