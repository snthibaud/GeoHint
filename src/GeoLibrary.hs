module GeoLibrary ( lineLineXn, lineCircleXn, circleCircleXn,
  explodeAngle,  perpendicularBisector2Points, parallelLine,
  parallelLineGivenLength, perpendicularToLineThruPoint,
  mirrorPointLine, concentricCircle, midpointGiven2Points,
  angularBisectorLines, distance, lineFromPoints )
where

import Data.Maybe (fromJust)
import DataTypes

infixl 6 |+|
(|+|) :: Vector -> Vector -> Vector
Vector a b |+| Vector c d = Vector (a+c) (b+d)

infixl 8 |.|
(|.|) :: Vector -> Vector -> Double
(Vector a b) |.| (Vector c d) = a * c + b * d

infixl 7 |*|
(|*|) :: Double -> Vector -> Vector
c |*| Vector a b = Vector (c*a) (c*b)

infixl 6 |-|
(|-|) :: Vector -> Vector -> Vector
a |-| b = a |+| ((-1) |*| b)

p2v :: Point -> Vector
p2v (Point px py) = Vector px py

v2p :: Vector -> Point
v2p (Vector vx vy) = Point vx vy

vectorLength :: Vector -> Double
vectorLength (Vector a b) = sqrt (a^2 + b^2)

normalizeVector :: Vector -> Vector
normalizeVector v = (1 / vectorLength v) |*| v

evaluateLine :: Line -> Length -> Point
evaluateLine (Line p v) l =
  v2p (p2v p |+| l |*| normalizeVector v)

lineLineXn :: Line -> Line -> Maybe Point
lineLineXn (Line p1 (Vector xx xy)) (Line p2 y)
  | yd /= 0 =
    Just (v2p (p2v p2 |+| ((p2v p1 |-| p2v p2) |.| d / yd) |*| y))
  | otherwise = Nothing
  where
    yd = y |.| d
    d = Vector (-xy) xx

lineCircleXn :: Line -> Circle -> [Point]
lineCircleXn (Line a b) (Circle c r)
    | e == 0 = [evaluateLine (Line a b) (d / f)]
    | e > 0 = [evaluateLine (Line a b) ((d - e) / f) ,
               evaluateLine (Line a b) ((d + e) / f)]
    | otherwise = []
    where
        f = 2
        (d, e) = (((-2) |*| (p2v a |-| p2v c)) |.| bn,
                  sqrt (((2 |*| (p2v a |-| p2v c)) |.| bn)^2 - 4
                  * (vectorLength (p2v a |-| p2v c)^2 - r^2)))
        bn = normalizeVector b


circleCircleXn :: Circle -> Circle -> [Point]
circleCircleXn (Circle a ra) (Circle b rb) =
    lineCircleXn radicalAxisLine (Circle a ra)
        where
            radicalAxisLine =
              Line radicalAxisPoint (Vector ((-1)*aby) abx)
            Vector abx aby = p2v b |-| p2v a
            radicalAxisPoint = v2p (p2v a
              |+| radicalAxisDistance
                |*| normalizeVector (p2v b |-| p2v a))
            radicalAxisDistance = (d + (ra^2 - rb^2) / d) / 2
            d = vectorLength (p2v b |-| p2v a)

explodeAngle :: Angle -> [Point]
explodeAngle (Angle a) =
  [Point 0 0, Point 0 1, Point (cos a) (sin a)]

perpendicularBisector2Points :: Point -> Point -> Line
perpendicularBisector2Points a b = lineFromPoints d e
    where
        [d, e] = circleCircleXn (Circle a c) (Circle b c)
        c = distance a b

parallelLine :: Line -> Point -> Line
parallelLine (Line a t) c
    | abs ((p2v g |-| p2v c) |.|  t)
      > abs ((p2v h |-| p2v c) |.| t) = Line c (p2v g |-| p2v c)
    | otherwise = Line c (p2v h |-| p2v c)
    where
        [g, h] = circleCircleXn x y
        y = Circle f (vectorLength (p2v d |-| p2v e))
        (d:l1, e:l2, f:l3) = (lineCircleXn (Line a t) z,
          lineCircleXn l z, lineCircleXn l x)
        l = Line a (p2v c |-| p2v a)
        z = Circle a 1
        x = Circle c 1

parallelLineGivenLength :: Line -> Length -> [Line]
parallelLineGivenLength l r = [parallelLine l c, parallelLine l d]
    where
        [c, d] = lineCircleXn f x
        x = Circle (fromJust e) r
        e = lineLineXn f l
        f = perpendicularBisector2Points
          (evaluateLine l 0) (evaluateLine l 1)

perpendicularToLineThruPoint :: Line -> Point -> Line
perpendicularToLineThruPoint l = parallelLine m
    where
        m = perpendicularBisector2Points
          (evaluateLine l 0) (evaluateLine l 1)

mirrorPointLine :: Point -> Line -> Point
mirrorPointLine a l = v2p (p2v a |+| 2 |*| (p2v b |-| p2v a))
    where
        b = fromJust
          (lineLineXn (perpendicularToLineThruPoint l a) l)

concentricCircle :: Circle -> Length -> Circle
concentricCircle (Circle m r) o = Circle m (r + o)

midpointGiven2Points :: Point -> Point -> Point
midpointGiven2Points a b = v2p ((1/2) |*| (p2v a |+| p2v b))

angularBisectorLines :: Line -> Line -> [Line]
angularBisectorLines (Line a d1) (Line b d2) = [Line c d3,
                                                Line c d4]
    where
        c = fromJust (lineLineXn (Line a d1) (Line b d2))
        d3 = normalizeVector (d1 |+| d2)
        d4 = normalizeVector (d1 |-| d2)

distance :: Point -> Point -> Length
distance p1 p2 = vectorLength (p2v p2 |-| p2v p1)

lineFromPoints :: Point -> Point -> Line
lineFromPoints p1 p2 = Line p1 (p2v p2 |-| p2v p1)
