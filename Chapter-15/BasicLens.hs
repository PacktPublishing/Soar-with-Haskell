data Lens' s v = MkLens' { view :: s -> v
                         , set  :: s -> v -> s }

data    Employee = MkEmployee { _firstName :: String
                              , _lastName  :: String
                              , _salary    :: Int
                              }
  deriving Show

director = MkEmployee "Parker" "Jones" 3500

salary :: Lens' Employee Int
salary = MkLens' { view = _salary
                 , set = \e s -> e { _salary = s } }


over :: Lens' s v -> (v -> v) -> (s -> s)
over l f s = set l s (f (view l s))

composeLens :: Lens' s m -> Lens' m v -> Lens' s v
composeLens l1 l2 = 
  MkLens' { view = view l2 . view l1
          , set = \s v -> set l1 s (set l2 (view l1 s) v)
          }

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
ceo = MkLens' { view = \(MkCompany _ c _) -> c
              , set = \(MkCompany n _ m) c -> MkCompany n c m
              }

