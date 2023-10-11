{-# LANGUAGE FlexibleInstances #-}

type Radius = Double

type Point = (Double, Double)

class Region r where
  circle  :: Radius -> r
  outside :: r -> r
  (/\)    :: r -> r -> r

instance Region (Point -> Bool) where
  circle r  = \(x,y) -> sqrt (x**2 + y ** 2) <= r
  outside r = \p -> not (r p)
  r1 /\ r2  = \p -> r1 p && r2 p

annulus :: Region r => Radius -> Radius -> r
annulus r1 r2 = outside (circle r1) /\ circle r2

inRegion :: Point -> (Point -> Bool) -> Bool
inRegion p r = r p

type Side = Double

class Square r where
  square :: Side -> r

someRegion :: (Region r, Square r) => Radius -> Side -> r
someRegion r s = circle r /\ outside (square s)

instance Square (Point -> Bool) where
  square s = \(x,y) -> abs x <= s/2 && abs y <= s/2
