import Control.Monad.Identity
import Control.Monad.Reader

type FederalTax = Float
type CityTax = Float

type M = ReaderT FederalTax (ReaderT CityTax Identity)

propertyTax :: Float -> M Float
propertyTax value = do fTax <- ask
                       cTax <- ask
                       pure (value * (fTax + cTax))

runM :: M a -> a
runM p = runIdentity (runReaderT (runReaderT p 0.10) 0.05)

