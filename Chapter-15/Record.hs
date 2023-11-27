data    Employee = MkEmployee { firstName :: String
                              , lastName  :: String
                              , salary    :: Int
                              }
  deriving Show

director = MkEmployee "Parker" "Jones" 3500

data Company = MkCompany { name  :: String
                         , ceo   :: Employee
                         , motto :: String
                         }
  deriving Show

fiberCo = 
  MkCompany 
    { name = "FiberCo"
    , ceo = MkEmployee { firstName = "Lux"
                       , lastName = "Cable"
                       , salary = 3000 }                       
    , motto = "We are the light at the end of your cable." }


