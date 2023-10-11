import Prelude hiding (or, truncate)

data Currency = EUR | GBP | JPY | USD

data Date = MkDate
  deriving Show

date :: String -> Date
date _ = MkDate

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
  deriving Show

expiry :: Contract -> ExpiryDate
expiry = undefined

value :: Contract -> Currency -> Date -> Double
value = undefined





-- instance S.Semiring (Contract obs) where
--   zero  = BlackHole
--   one   = Zero
--   plus  = Or
--   times = And
-- 
-- instance Multiplicative (Contract obs) where
--   timesInverse = Give
-- 
-- -- | A contract that has no rights or obligations.
-- --
-- -- It has an infinite horizon.
-- -- It is the multiplicative identity, 'S.one'.
-- zero :: Contract obs
-- zero = Zero
-- 
-- -- | A contract that gives exactly one unit of the specified currency to the
-- -- holder of the contract.
-- --
-- -- It has an infinite horizon.
-- one :: Currency -> Contract obs
-- one = One
-- 
-- -- | Choose to acquire either of two contracts, but not both.
-- --
-- -- Its horizon is the greater of the two contracts' horizons.
-- -- It is the addition for contracts ('S.plus').
-- or :: Contract obs -> Contract obs -> Contract obs
-- or = Or
-- 
-- -- | Acquire both contracts.
-- --
-- -- Its horizon is the lesser of the two contracts' horizons.
-- -- It is the multiplication for contracts ('S.times').
-- and :: Contract obs -> Contract obs -> Contract obs
-- and = And
-- 
-- -- | Give a contract to the other party.
-- --
-- -- This transfers your obligations to the other party and vice-versa.
-- -- Its horizon is the horizon of the underlying contract.
-- --
-- -- It calculates multiplicative inverses ('timesInverse').
-- give :: Contract obs -> Contract obs
-- give = Give
-- 
-- -- | Truncate the contract's horizon to the given time.
-- --
-- -- Its horizon is the lesser of the given time and the contract's horizon.
-- truncate :: Time -> Contract obs -> Contract obs
-- truncate = Truncate
-- 
-- -- | Follow one contract by another.
-- -- 
-- -- If you acquire @followedBy c1 c2@, you acquire @c1@, if it has
-- -- not yet expired. If it has, and @c2@ has not, you acquire @c@.
-- --
-- -- The horizon is the greater of the two contracts' horizons.
-- followedBy :: Contract obs -> Contract obs -> Contract obs
-- followedBy = FollowedBy
-- 
-- -- | Modify a contract by an observable.
-- --
-- -- If you acquire this contract, you acquire the rights and obligations of the
-- -- underlying contract, modified by the observable at the time of acquisition.
-- --
-- -- The horizon is the horizon of the underlying contract.
-- scale :: obs -> Contract obs -> Contract obs
-- scale = Scale
-- 
-- -- | Get a contract at its horizon.
-- --
-- -- This contract requires you to obtain the underlying contract at its horizon.
-- --
-- -- The horizon is the horizon of the underlying contract.
-- get :: Contract obs -> Contract obs
-- get = Get
-- 
-- -- | Get a contract at any time before or at its horizon.
-- --
-- -- If you acquire @anytime c@, you are required to obtain @c@, but can do so
-- -- at any time between acquiring @c@ and @c@'s horizon.
-- --
-- -- The horizon is the horizon of the underlying contract.
-- anytime :: Contract obs -> Contract obs
-- anytime = Anytime
-- 
-- -------------------------------------------------------------------------------
-- -- Horizon
-- 
-- -- | Determine the time horizon of a contract (i.e. the latest time before
-- -- which it can be acquired).
-- --
-- -- Note that this is a semiring morphism, i.e.,
-- --
-- -- prop> horizon S.zero = horizon BlackHole = Finite 0 = S.zero
-- -- prop> horizon S.one  = horizon Zero = Infinite = S.one
-- -- prop> horizon (c1 `S.plus` c2) = horizon c1 `S.plus` horizon c2
-- -- prop> horizon (c1 `S.times` c2) = horizon c1 `S.times` horizon c2
-- --
-- -- for any contracts @c1@ and @c2@.
-- --
-- horizon :: Contract obs -> Time
-- horizon BlackHole          = Finite 0 -- S.zero
-- horizon Zero               = Infinite -- S.one
-- horizon One{}              = Infinite
-- horizon (Or c1 c2)         = horizon c1 `S.plus` horizon c2
-- horizon (And c1 c2)        = horizon c1 `S.times` horizon c2
-- horizon (Give c)           = horizon c
-- horizon (Truncate t _)     = t
-- horizon (FollowedBy c1 c2) = horizon c1 `S.plus` horizon c2
-- horizon (Scale _ c)        = horizon c
-- horizon (Get c)            = horizon c
-- horizon (Anytime c)        = horizon c
-- 
-- -------------------------------------------------------------------------------
-- -- Worth
-- 
-- type Worth = Max Double
-- type Exchange sg = Currency -> Time -> sg
-- type Valuation obs sg = obs -> Time -> sg -> sg
-- 
-- -- | The worth of a contract (i.e. the value of a contract if it were to be
-- -- acquired at a certain point in time).
-- --
-- -- In what follows, assume that all exchanges and valuations never return
-- -- 'S.zero'.
-- --
-- -- Property: For a contract @c@ with horizon @h@, any valuation @v@,
-- -- exchange @e@ and time @t@:
-- --
-- -- prop> worth contract e v t /= S.zero iff t < horizon c
-- --
-- -- Thus we have an alternative definition of 'horizon', where the horizon
-- -- of a contract @c@ is the earliest time @t@ such that there exists an
-- -- exchange @e@, such that @worth contract e t = S.zero@.
-- --
-- -- Notice that if there exists an @e@ and @v@,
-- -- such that @worth contract e v t /= S.zero@,
-- -- then for all @v'@ and @e'@, @worth contract v' e' t /= S.zero@.
-- --
-- -- This function is an multiplicative semiring morphism, i.e., for any contracts
-- -- @c1@ and @c2@:
-- --
-- -- prop> worth S.zero = worth BlackHole = S.zero
-- -- prop> worth S.one = worth Zero = S.one
-- -- prop> worth (c1 `S.times` c2) = worth (And c1 c2) = worth c1 `S.times` worth c2
-- -- prop> worth (c1 `S.plus` c2) = worth (Or c1 c2) = worth c1 `S.plus` worth c2
-- --
-- -- And for @c@ s.t. @c /= S.zero@:
-- -- prop> worth (timesInverse c) = worth (Give c) = timesInverse (worth c)
-- --
-- worth
--   :: (Multiplicative sg, Discounted sg)
--   => Contract obs -> Valuation obs sg -> Exchange sg -> Time -> sg
-- worth BlackHole _valuation _exchange _time =
--   S.zero -- a black hole is worse than worthless: it eats all your money
--   
-- worth Zero _valuation _exchange _time =
--   S.one
--   
-- worth (One c) _valuation exchange time =
--   exchange c time
--   
-- worth (Or c1 c2)  valuation exchange time =
--   worth c1 valuation exchange time `S.plus` worth c2 valuation exchange time
--     
-- worth (And c1 c2) valuation exchange time =
--   worth c1 valuation exchange time `S.times` worth c2 valuation exchange time
--     
-- worth (Give c) valuation exchange time
--   | time < horizon c = timesInverse (worth c valuation exchange time)
--   | otherwise = S.zero
--   
-- worth (Truncate t0 c) valuation exchange time
--   | time < (horizon c `S.times` t0) = worth c valuation exchange time
--   | otherwise = S.zero
--   
-- worth (FollowedBy c1 c2) valuation exchange time
--   | time < horizon c1 = worth c1 valuation exchange time
--   | otherwise = worth c2 valuation exchange time
--   
-- worth (Scale observable c) valuation exchange time =
--   valuation observable time (worth c valuation exchange time)
--   
-- worth (Get c) valuation exchange time = case horizon c of
--   Infinite -> error "Cannot get a contract with an infinite horizon."
--   h@Finite{}
--     | time < h, Just h' <- previous h, Just dTime <- delta time h' ->
--         discount dTime (worth c valuation exchange h')
--     | otherwise -> S.zero
-- 
-- worth (Anytime c) valuation exchange time = case horizon c of
--   Infinite ->
--     error "anytime contract of a contract with an infinite horizon"
--   h@Finite{}
--     | time < h, Just h' <- previous h ->
--         let discounted t = discount dTime worthAtT where
--               worthAtT = worth (Anytime c) valuation exchange t
--               Just dTime = delta time t
--             w = worth c valuation exchange time
--         in S.ssum (w:[discounted between | between <- drop 1 (steps time h')])
--         -- This kind of computes a snell envelope. The sum of this list dominates
--         -- the worth @w@ of @c@ at the current time, and the worth of @Anytime c@
--         -- at any later point, up to the horizon @h@.
--     | otherwise -> S.zero
--   
-- 
-- -------------------------------------------------------------------------------
-- -- Ho-Lee lattice based model of evolution of inflation.
-- 
-- -- | Discount a future value backwards in time.
-- class Discounted v where
--   -- | Discount a value.
--   discount
--     :: Int -- ^ number of timesteps
--     -> v -- ^ value in the future to discount
--     -> v -- ^ discounted value
-- 
-- instance Discounted (Max Double) where
--   discount n = fmap (fromRational . hoLee n 0.05 0.01 . toRational)
-- 
-- -- | The evolution of the interest according to the Ho-Lee lattice model.
-- --
-- -- Note that this model does in fact have a closed-from solution.
-- --
-- -- The function takes a number of time steps to simulate, and an inital rate.
-- -- It then adds or subtracts @0.01@ from this rate in every time step.
-- -- The result is a list of distributions over interest rates, where each
-- -- distribution corresponds to the situtation in a particular time slice.
-- --
-- -- Examples:
-- --
-- -- >>> mapM_ (tabulate "Rate" "Probability") (interestEvolution 0 0.07)
-- -- > Rate   | Probability
-- -- > -------+------------
-- -- > 7.0e-2 | 1.0
-- -- >
-- -- >>> mapM_ (tabulate "Rate" "Probability") (interestEvolution 2 0.07)
-- -- > Rate   | Probability
-- -- > -------+------------
-- -- > 9.0e-2 | 0.25       
-- -- > 7.0e-2 | 0.5        
-- -- > 5.0e-2 | 0.25       
-- -- >
-- -- > Rate                  | Probability
-- -- > ----------------------+------------
-- -- > 8.0e-2                | 0.5        
-- -- > 6.0000000000000005e-2 | 0.5        
-- -- > 
-- -- > Rate   | Probability
-- -- > -------+------------
-- -- > 7.0e-2 | 1.0
-- --
-- interestEvolution
--    :: (Ord a, Num a, IsProbability sg, Multiplicative sg)
--    => Int -> a -> a -> [Dist sg a]
-- interestEvolution n rate drate
--   | n <= 0 = [return rate]
--   | otherwise =
--       let evolution = interestEvolution (n - 1) rate drate
--           slice = normalise $ do
--             r <- head evolution
--             categorical [(prob 0.5, r - drate), (prob 0.5, r + drate)]
--       in slice : evolution
-- 
-- -- | Discount a value over a number of time steps according to a given rate.
-- --
-- -- Given a number of time steps and an initial rate, and the future value,
-- -- this function discounts the value back in time to the present. The interest
-- -- rate is evolved according to the 'interestEvolution' model.
-- --
-- -- Example (discounting the value 100 over two timesteps, with an initial rate
-- -- of 7 percent and changing the rate by +/- 0.01 percent every step):
-- --
-- -- >>> hoLee 2 0.07 0.01 100
-- -- 87.35150244584206
-- --
-- hoLee
--   :: forall a.(Ord a, Fractional a) => Int -> a -> a -> a -> a
-- hoLee n rate0 drate worth0 =
--   let dist = interestEvolution n rate0 drate :: [Dist Double a]
--       evolution = map (map snd . runDist) dist
--       discAvg rate v1 v2 = (v1 + v2) / (2 + 2*rate)
--         -- discV1 = v1 / (1 + r)
--         -- discV2 = v2 / (1 + r)
--         -- discAvg = (discV1 + discV2) / 2
--         --         = (v1 + v2) / (2 + 2*rate)
--       discounted :: [a] -> [a] -> [a]
--       discounted [worth] _ = [worth]
--       discounted worths slice = zipWith3 discAvg slice worths (tail worths)
--   in head $ foldl' discounted (replicate (n + 1) worth0) (drop 1 evolution) 
