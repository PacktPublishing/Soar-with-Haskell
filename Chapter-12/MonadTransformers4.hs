{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

data LogLevel = Mundane | Important | Critical
  deriving (Eq, Ord, Show)

mundane, important, critical :: String -> App ()
mundane   = log Mundane
important = log Important
critical  = log Critical

data Config = 
        MkConfig { shouldLog :: LogLevel -> Bool
	         , output :: (LogLevel, String) -> IO ()}

defaultCfg :: Config
defaultCfg = MkConfig { shouldLog = \lvl -> True
                      , output    = print }

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

type App = ReaderT Config IO

log :: LogLevel -> String -> App ()
log lvl msg = 
  do p <- asks shouldLog
     when (p lvl)
       (do out <- asks output 
           lift (out (lvl,msg)))

runApp :: Config -> App a -> IO a
runApp cfg app = runReaderT app cfg
