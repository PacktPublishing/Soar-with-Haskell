type Radius = Double

type Point = (Double, Double)

data Region
  = Circle Radius
  | Outside Region
  | Region :/\ Region

circle :: Radius -> Region
circle  = Circle

outside :: Region -> Region
outside = Outside

(/\) :: Region -> Region -> Region
(/\)    = (:/\)

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2

inRegion :: Point -> Region -> Bool
(x,y) `inRegion` Circle r    =  sqrt (x**2 + y ** 2) <= r
p     `inRegion` Outside r   =  not (p `inRegion` r)
p     `inRegion` (r1 :/\ r2) =  p `inRegion` r1 && p `inRegion` r2
