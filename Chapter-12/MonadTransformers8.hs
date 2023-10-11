import Control.Monad.Identity
import Control.Monad.Reader

type FederalTax = Float
type CityTax = Float

data Taxes = MkTaxes { federalTax :: Float
                     , cityTax    :: Float }

type M = ReaderT Taxes Identity

runM :: M a -> a
runM p = runIdentity (runReaderT p (MkTaxes 0.10 0.05))

propertyTax :: Float -> M Float
propertyTax value = do fTax <- asks federalTax
                       cTax <- asks cityTax
                       pure (value * (fTax + cTax))
