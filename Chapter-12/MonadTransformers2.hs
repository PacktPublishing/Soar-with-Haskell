{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (log)
import Control.Monad

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

newtype App a = MkApp { unApp :: Config -> IO a }
  deriving Functor

log :: LogLevel -> String -> App ()
log lvl msg = MkApp (\cfg ->
  do when (shouldLog cfg lvl)
       (output cfg (lvl,msg)))

runApp :: Config -> App a -> IO a
runApp cfg app = unApp app cfg

instance Monad App where
  p >>= f = MkApp (\cfg -> 
    do x <- runApp cfg p
       runApp cfg (f x))

instance Applicative App where
  pure x = MkApp (\cfg -> pure x)
  (<*>) = ap
