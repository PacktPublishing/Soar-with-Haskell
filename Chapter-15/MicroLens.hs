{-# LANGUAGE RankNTypes #-}

import Data.Functor.Identity
import Data.Functor.Const

type Lens' s v = 
  forall f. Functor f => (v -> f v) -> (s -> f s)

over :: Lens' s v -> (v -> v) -> (s -> s)
over l f s = runIdentity (l (Identity . f) s)

set :: Lens' s v -> v -> (s -> s)
set l v s = over l (\_ -> v) s

view :: Lens' s v -> s -> v
view l s = getConst (l Const s)


data    Employee = MkEmployee { _firstName :: String
                              , _lastName  :: String
                              , _salary    :: Int
                              }
  deriving Show

director = MkEmployee "Parker" "Jones" 3500

salary :: Lens' Employee Int
salary t (MkEmployee f l s) = 
  fmap (\s' -> MkEmployee f l s') (t s)

data Company = MkCompany { _name  :: String
                         , _ceo   :: Employee
                         , _motto :: String
                         }
  deriving Show

fiberCo = 
  MkCompany 
    { _name = "FiberCo"
    , _ceo = MkEmployee { _firstName = "Lux"
                        , _lastName = "Cable"
                        , _salary = 3000 }                       
    , _motto = "We are the light at the end of your cable." }

ceo :: Lens' Company Employee
ceo t (MkCompany n c m) =
  fmap (\c' -> MkCompany n c' m) (t c) 

(^.) :: s -> Lens' s v -> v
s ^. l = view l s

infixr 4 .~

(.~) :: Lens' s v -> v -> (s -> s)
l .~ v = set l v

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

(%~) :: Lens' s v -> (v -> v) -> (s -> s)
l %~ f = over l f

(+~) :: Num v => Lens' s v -> v -> (s -> s)
l +~ x = l %~ (+x)

(-~) :: Num v => Lens' s v -> v -> (s -> s)
l -~ x = l %~ (subtract x)

(<>~) :: Semigroup v => Lens' s v -> v -> (s -> s)
l <>~ x = l %~ (<> x)

