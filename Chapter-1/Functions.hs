
-- # 1.2.1 Our First Function

increment :: Int -> Int
increment x = x + 1

-- # 1.2.2 A Two-Parameter Function

average :: Float -> Float -> Float
average a b = (a + b) / 2

-- # 1.2.3 Custom Operators

(><) :: Float -> Float -> Float
a >< b = (a + b) / 2

average' :: Float -> Float -> Float
average' a b = (/) ((+) a b) 2

-- # 1.3.3 Booleans

-- ## Comparisons

passing :: Float -> Bool
passing grade = grade >= 5.0

-- ## Boolean Combinators

workingAge :: Int -> Bool
workingAge age = 15 <= age && age <= 64

-- ## If-then-else

discount :: Int -> Float
discount qty = if qty >= 10 then 0.10 else 0

discount2 :: Int -> Float
discount2 qty = if qty >= 50 then 0.15 
                  else if qty >= 10 then 0.10 else 0

-- ## Guards

discount2' :: Int -> Float
discount2' qty
  | qty >= 50  = 0.15 
  | qty >= 10  = 0.10
  | otherwise  = 0

-- ## Exhaustive Cases

missingCase :: Int -> Float
missingCase qty
  | qty >= 50  = 0.15
  | qty >= 10  = 0.10

-- ## Indentation Sensitive Syntax

{- This code is syntactically invalid:

discount2'’ :: Int -> Float
discount2’' qty
| qty >= 50  = 0.15 
| qty >= 10  = 0.10
| otherwise  = 0

-}

-- # 1.3.4 Char and String

capitalA :: Char
capitalA = 'A'

emptyString :: String
emptyString = ""

hello :: String
hello = "Hello, World!"

-- # 1.5.1 Calling Functions from within Functions

price :: Float -> Int -> Float
price ip qty = ip * fromIntegral qty

reducePercentage :: Float -> Float -> Float
reducePercentage pct p = (1 - pct) * p

discountedPrice :: Float -> Int -> Float
discountedPrice ip qty = 
  reducePercentage (discount qty) (price ip qty)

-- # 1.5.2 Naming Intermediate Results

discountedPrice2 :: Float -> Int -> Float
discountedPrice2 ip qty = 
  let p   = price ip qty
      pct = discount qty
  in reducePercentage pct p

discountedPrice3 :: Float -> Int -> Float
discountedPrice3 ip qty =
  reducePercentage pct p
    where p   = price ip qty
          pct = discount qty

-- # 1.5.3 Local Function Definitions

discountedPrice4 :: Float -> Int -> Float
discountedPrice4 ip qty = 
  reducePercentage (discount qty) (price ip qty)
    where 
      reducePercentage :: Float -> Float -> Float
      reducePercentage pct p = (1 - pct) * p



