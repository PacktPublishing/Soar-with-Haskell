{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative

type M1 = StateT Int (MaybeT Identity)
type M2 = MaybeT (StateT Int Identity)

runM1 :: M1 a -> Int -> Maybe (a, Int)
runM1 p s = runIdentity (runMaybeT (runStateT p s))

runM2 :: M2 a -> Int -> (Maybe a, Int)
runM2 p s = runIdentity (runStateT (runMaybeT p) s)


p :: (MonadState Int m) => m ()
p = do s <- get
       put (s + 1)

