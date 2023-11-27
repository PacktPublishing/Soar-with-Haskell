{-# LANGUAGE FlexibleContexts #-}

import Lens.Micro
import Lens.Micro.GHC

import Data.Map



data    Employee = MkEmployee { _firstName :: String
                              , _lastName  :: String
                              , _salary    :: Int
                              }
  deriving Show

director = MkEmployee "Parker" "Jones" 3500

names :: Traversal Employee Employee String String
names t (MkEmployee f l s) = 
  (\f' l' -> MkEmployee f' l' s) <$> t f <*> t l

salary :: Lens' Employee Int
salary t (MkEmployee f l s) =
  (\s' -> MkEmployee f l s') <$> t s

data Department = MkDepartment { _deptHead :: Employee, _deptStaff :: [Employee] }
  deriving Show

staff :: Lens' Department [Employee]
staff t (MkDepartment h s) =
  (MkDepartment h) <$> t s

sales :: Department 
sales = MkDepartment director [director, director, director]


data Shape = Circle Double 
           | Rectangle Double Double

radius :: Traversal' Shape Double
radius t (Circle r) = Circle <$> t r
radius t s          = pure s


