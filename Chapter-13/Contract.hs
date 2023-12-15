import Prelude hiding (or, truncate)
import Data.Time 

data Currency = EUR | GBP

data Date = MkDate Integer
  -- MkDate 0 is the "epoch", the reference date.
  -- We use the underlying representation of the Day
  -- type from the Data.Time library
  deriving (Eq, Ord)

instance Show Date where
  show (MkDate d) = show (ModifiedJulianDay d)

date :: String -> Date
date s = 
  let timeFromString = parseTimeOrError True defaultTimeLocale "%e %b %Y" s :: UTCTime
  in MkDate (toModifiedJulianDay (utctDay timeFromString))

data Contract
  = One Currency
  | Truncate Date Contract
  | Get Contract
  | Scale Double Contract
  | Give Contract
  | Both Contract Contract
  | Or Contract Contract
  | Thereafter Contract Contract
  | Anytime Contract

one :: Currency -> Contract
one = One

truncate :: Date -> Contract -> Contract
truncate = Truncate

get :: Contract -> Contract
get = Get

scale :: Double -> Contract -> Contract
scale = Scale

zcb :: Date -> Double -> Currency -> Contract
zcb date amount currency = 
  scale amount (get (truncate date (one currency)))

c11 :: Contract
c11 = zcb (date "29 Jan 2025") 100 EUR

zero :: Contract
zero = scale 0 (one EUR)

give :: Contract -> Contract
give = Give

c12 :: Contract
c12 = give (zcb (date "1 Feb 2026") 105 EUR)

both :: Contract -> Contract -> Contract
both = Both

c1 :: Contract
c1 = both c11 c12

or :: Contract -> Contract -> Contract
or = Or

european :: Date -> Contract -> Contract
european d c = get (truncate d (c `or` zero))

thereafter :: Contract -> Contract -> Contract
thereafter = Thereafter

anytime :: Contract -> Contract
anytime = Anytime

american :: Date -> Date -> Contract -> Contract
american d1 d2 c = before `thereafter` after where
  before = get (truncate d1 after)
  after  = anytime (truncate d2 (c `or` zero))

data ExpiryDate = ExpiresOn Date | ExpiresNever
  deriving (Eq, Ord, Show)

expiry :: Contract -> ExpiryDate
expiry (One cur)           = ExpiresNever
expiry (Truncate d c)      = ExpiresOn d `min` expiry c
expiry (Get c)             = expiry c
expiry (Scale k c)         = expiry c
expiry (Give c)            = expiry c
expiry (Both c1 c2)        = expiry c1 `min` expiry c2
expiry (Or c1 c2)          = expiry c1 `max` expiry c2
expiry (Thereafter c1 c2)  = expiry c1 `max` expiry c2
expiry (Anytime c)         = expiry c

value :: Contract -> Currency -> Date -> Double
value c cur d = go c where
  go :: Contract -> Double 
  go c 
   | ExpiresOn d >=  expiry c  = 0.0
  go (One cur')                = exch cur cur'
  go (Truncate d c)            = go c
  go (Get c)                   
    | Just d' <- horizon c     = disc d' (go c) d
    | otherwise                = 0.0
  go (Scale k c)               = k * go c
  go (Give c)                  = negate (go c)
  go (Both c1 c2)              = go c1 + go c2
  go (Or c1 c2)                = go c1 `max` go c2
  go (Thereafter c1 c2)   
    | ExpiresOn d < expiry c1  = go c1
    | otherwise                = go c2
  go (Anytime c)         
    | Just _ <- horizon c      = snell (value c cur) d
    | otherwise                = 0.0
  
  exch :: Currency -> Currency -> Double
  exch EUR GBP = 1.4
  exch GBP EUR = recip 1.4
  exch _   _   = 1

  horizon :: Contract -> Maybe Date
  horizon = previous . expiry

  previous :: ExpiryDate -> Maybe Date
  previous (ExpiresOn (MkDate d)) 
    | d > 0 = Just (MkDate (d-1))
  previous _ = Nothing

  next :: Date -> Date
  next (MkDate d) = MkDate (d+1)

  disc :: Date -> Double -> Date -> Double
  disc (MkDate h) v (MkDate d)
   = v / (1 + rate**(fromIntegral (h - d) / 365))

  snell :: (Date -> Double) -> Date -> Double
  snell f d 
    | f d > negInfty = f d `max` (disc (next d) (snell f (next d)) d)
    | otherwise      = negInfty

    -- assumed fixed interest rate
  rate :: Double 
  rate = 0.03

  negInfty :: Double
  negInfty = -1/0
