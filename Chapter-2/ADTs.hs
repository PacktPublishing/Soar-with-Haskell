{-# OPTIONS_GHC -W #-}
-- {-# LANGUAGE OverloadedRecordDot #-}

-- 2.1.1 A Game of Rock-Paper-Scissors

data Outcome = Lose | Draw | Win

render :: Outcome -> String
render Lose = "lose"
render Draw = "draw"
render Win  = "win"

data Gesture = Rock | Paper | Scissors

play :: Gesture -> Gesture -> Outcome
play Rock     Rock     = Draw
play Rock     Paper    = Lose
play Rock     Scissors = Win
play Paper    Rock     = Win
play Paper    Paper    = Draw
play Paper    Scissors = Lose
play Scissors Rock     = Lose
play Scissors Paper    = Win
play Scissors Scissors = Draw

{- Uncomment this definition and comment the above one
   to see which cases are missing from the play function.

data Gesture = Rock | Paper | Scissors | Lizard | Spock

-}

-- #  2.1.2 Don’t Care Patterns

play' :: Gesture -> Gesture -> Outcome
play' Rock     Rock     = Draw
play' Rock     Scissors = Win
play' Paper    Rock     = Win
play' Paper    Paper    = Draw
play' Scissors Paper    = Win
play' Scissors Scissors = Draw
play' _        _        = Lose

-- # 2.1.3 Booleans Revisited

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

boolToInt' :: Bool -> Int
boolToInt' b = if b then 1 else 0

xor :: Bool -> Bool -> Bool
xor False False = False
xor False True  = True
xor True  False = True
xor True  True  = False

data Light = Off | On

toggle :: Light -> Light
toggle Off = On
toggle On  = Off

-- # 2.2.1 People

data Person = MkPerson String Int

tom :: Person
tom = MkPerson "Tom" 45

age :: Person -> Int
age (MkPerson n a) = a

age' :: Person -> Int
age' (MkPerson _ a) = a

data Person' = Person' String Int

-- # 2.2.2 Named Fields

data Point' = MkPoint' Double Double

data Point = MkPoint { xcoord :: Double, ycoord :: Double }

project :: Point -> Point
project (MkPoint x _) = MkPoint x 0

project2 :: Point -> Point
project2 (MkPoint {xcoord=x}) = MkPoint {ycoord = 0, xcoord = x}

project3 :: Point -> Point
project3 p = MkPoint {ycoord = 0, xcoord = xcoord p}

-- project4 :: Point -> Point
-- project4 p = MkPoint {ycoord = 0, xcoord = p.xcoord}

project5 :: Point -> Point
project5 p = p {ycoord = 0}

-- 2.2.3 Nested Records

data Company = MkCompany { name     :: String
                         , manager  :: Person
                         , location :: Point
                         }

managerAge :: Company -> Int
managerAge (MkCompany _ (MkPerson _ a) _) = a

managerAge' :: Company -> Int
managerAge' c = age (manager c)

-- # 2.3.1 Shapes

data Shape = Circle Double 
           | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r**2
area (Rectangle w h) = w * h

circumference :: Shape -> Double
circumference (Circle r)      = 2 * pi * r
circumference (Rectangle w h) = 2 * (w + h)

pictogram :: Shape -> Char
pictogram (Circle _)      = '◯'
pictogram (Rectangle _ _) = '▭'

data Shape' = Circle' { radius :: Double }
            | Rectangle' { width :: Double, height :: Double }

-- # 2.3.2 Cards

data Suit = Clubs | Spades | Diamonds | Hearts
  deriving Show

data Rank = Numeral Int | Face Court
  deriving Show

data Court = Jack | Queen | King
  deriving Show

data Card = MkCard Suit Rank | Joker
  deriving Show

aceOfSpades :: Card
aceOfSpades = MkCard Spades (Numeral 1)

-- # 2.4.1 The Identity Function

idInt :: Int -> Int
idInt x = x

idBool :: Bool -> Bool
idBool x = x

idChar :: Char -> Char
idChar x = x

id :: a -> a
id x = x

-- # 2.4.2 The Constant Function

const :: a -> b -> a
const x _ = x

-- # 2.5.1 Tuples 

fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd (_,y) = y

-- # 2.5.2 Type Synonyms

project' :: (Double,Double) -> (Double,Double)
project' (x,_) = (x,0)

type PointSyn = (Double,Double)

project'' :: PointSyn -> PointSyn
project'' (x,_) = (x,0)

-- # 2.5.3 Maybe

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y
  | y == 0    = Nothing
  | otherwise = Just (div x y)

incrementMaybe :: Maybe Integer -> Maybe Integer
incrementMaybe Nothing = Nothing
incrementMaybe (Just x) = Just (x + 1)

-- # 2.5.4 Either

data TransferError = NonPositiveAmount | InsufficientFunds

data Account = MkAccount { owner :: String, amount :: Double }

transfer :: Account -> Account -> Double
  -> Either TransferError (Account,Account)
transfer from to amt
  | amt <= 0
  = Left NonPositiveAmount
  | amount from < amt
  = Left InsufficientFunds
  | otherwise
  = Right (from { amount = amount from - amt }
          ,to   { amount = amount to   + amt }
          ) 

-- # 2.5.4 The Unit Type

eitherToMaybe :: Either () a -> Maybe a
eitherToMaybe (Left ()) = Nothing
eitherToMaybe (Right x) = Just x

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing  = Left () 
maybeToEither (Just x) = Right x





