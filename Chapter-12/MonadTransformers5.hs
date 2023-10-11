{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader
import Control.Monad.Writer

type VAT = Float

priceWithVAT :: MonadReader VAT m => Float -> m Float
priceWithVAT p = do vat <- ask 
                    pure (p * (1 + vat))

orderWidgets 
  :: (MonadReader VAT m, MonadWriter [String] m) 
   => Int -> m Float
orderWidgets n = do tell ["Order of " ++ show n ++ " widgets"] 
                    p <- priceWithVAT 5.2
                    tell ["  at unit price " ++ show p]
                    pure (fromIntegral n * p)

