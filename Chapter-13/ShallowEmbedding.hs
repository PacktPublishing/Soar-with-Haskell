type Radius = Double

type Point = (Double, Double)

type Region = Point -> Bool

circle :: Radius -> Region
circle r  = \(x,y) -> sqrt (x**2 + y ** 2) <= r

outside :: Region -> Region
outside r = \p -> not (r p)

(/\) :: Region -> Region -> Region
r1 /\ r2  = \p -> r1 p && r2 p

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2

inRegion :: Point -> Region -> Bool
p `inRegion` r = r p

type Side = Double

square :: Side -> Region
square s = \(x,y) -> abs x <= s/2 && abs y <= s/2
