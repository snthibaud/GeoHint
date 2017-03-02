{-# LANGUAGE NoMonomorphismRestriction #-}
module Experiment (
    experimentMain
) where

import Diagrams.Prelude hiding (Dynamic, distance, Context, square, Line, Point, trace, hexagon)
import Data.List (sortBy, sort, intercalate)
import Data.Ord (comparing)
import Diagrams.Backend.SVG
import qualified DataTypes
import Data.Maybe (fromJust, isNothing)
import GeoLibrary
import Strategies (programToStrategy)
import Programs (square, hexagon, triangleGiven2SidesAndIncludedAngle, tangentArcsToTwoCircles)
import Rules
import Ideas.Common.Exercise hiding (makeExercise)
import Ideas.Common.Library hiding (Context, (#), makeExercise)
import Ideas.Service.Types
import Ideas.Service.ServiceList
import Ideas.Service.DomainReasoner
import Ideas.Common.Utils
import Ideas.Main.Default (defaultMain)
import Data.Bool
import Ideas.Common.Rewriting.Term
import DataTypes
import qualified Data.Text as T
import Debug.Trace
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Drawing = Diagram B

class Drawable a where
  draw :: a -> Drawing -> Drawing

determinePointSize :: Drawing -> Double
determinePointSize d = max (min (width d) (height d) / 50.0) 0.02

drawCircle :: DataTypes.Circle -> Drawing
drawCircle (DataTypes.Circle (DataTypes.Point px py) r) = circle r # lc forestgreen # translate (r2 (px, py))

instance Drawable DataTypes.Circle where
  draw c d = drawCircle c `atop` d

instance Drawable DataTypes.Line where-- import Rules (lineLineXnRule, execute)
  draw (DataTypes.Line (DataTypes.Point px py) (DataTypes.Vector vx vy)) d
    | isNothing minp || isNothing maxp = d
    | otherwise = strokeLocLine (lineFromVertices [fromJust minp, fromJust maxp] `at` fromJust minp) # lc royalblue `atop` d
    where
      minp = traceP (p2 (px, py)) (r2 (vx, vy)) d
      maxp = maxTraceP (p2 (px, py)) (r2 (vx, vy)) d

instance Drawable DataTypes.Point where
  draw p d = drawCircle (DataTypes.Circle p (determinePointSize d)) # fc red # lc red `atop` d


drawMany :: (Drawable a) => Drawing -> [a] -> Drawing
drawMany = foldr draw
--
drawAll :: ([DataTypes.Point], [DataTypes.Line], [DataTypes.Circle]) -> Drawing
drawAll (ps, ls, cs) = drawMany (drawMany (drawMany mempty cs) ls) ps

similarityExpr :: Program -> Program -> Bool
similarityExpr e1 e2 = fst e1 == fst e2

eqExpr :: Program -> Program -> Bool
eqExpr e1 e2 = True

separateObjects :: [Object] -> ([DataTypes.Point], [DataTypes.Line], [DataTypes.Circle])
separateObjects (PointObject x:xs) = (x:points, lines, circles)
 where
   (points, lines, circles) = separateObjects xs
separateObjects (LineObject x:xs) = (points, x:lines, circles)
 where
   (points, lines, circles) = separateObjects xs
separateObjects (CircleObject x:xs) = (points, lines, x:circles)
 where
   (points, lines, circles) = separateObjects xs
separateObjects (_:xs) = separateObjects xs
separateObjects [] = ([], [], [])

renderExpr :: Program -> String
renderExpr e = show $ renderDia SVG (SVGOptions (dims2D 250 250) Nothing (T.pack "") [] False)
  (drawAll . separateObjects $ map snd (snd e))

makeExercise :: String -> String -> LabeledStrategy Program -> Exercise Program
makeExercise description label program = emptyExercise
   { exerciseId    = describe description $
                        newId label
   , strategy      = liftToContext program
   , equivalence   = withoutContext eqExpr
   , similarity   = withoutContext similarityExpr
   , prettyPrinter = show
  --  , prettyPrinter = renderExpr
   , parser = readM
   , examples = [(Medium, ([], []))]
   }


squareExercise :: Exercise Program
squareExercise = makeExercise "Build a square" "eval.square" (programToStrategy square "square")

hexagonExercise = makeExercise "Build a hexagon" "eval.hexagon" (programToStrategy hexagon "hexagon")

triangleGiven2SidesAndIncludedAngleExercise = makeExercise
  "Build triangle using two given sides and an included angle" "eval.triangleGiven2SidesAndIncludedAngle"
  (programToStrategy triangleGiven2SidesAndIncludedAngle "triangleGiven2SidesAndIncludedAngle")

tangentArcsToTwoCirclesExercise = makeExercise
  "Build the arcs (circles) of specified radius that are tangent to two given circles" "eval.tangentArcsToTwoCircles"
  (programToStrategy tangentArcsToTwoCircles "tangentArcsToTwoCircles")

myServices :: [Service]
myServices = metaServiceList dr ++ serviceList

dr :: DomainReasoner
dr = describe "Domain reasoner for GeoHint" (newDomainReasoner "eval")
   { exercises = [Some squareExercise, Some hexagonExercise, Some triangleGiven2SidesAndIncludedAngleExercise,
      Some tangentArcsToTwoCirclesExercise]
   , services  = myServices
   , scripts = [ (getId squareExercise, "square_feedforward.txt")
               , (getId hexagonExercise, "hexagon_hints.txt")
               , (getId triangleGiven2SidesAndIncludedAngleExercise, "triangleGiven2SidesAndIncludedAngle_hints.txt")
               , (getId tangentArcsToTwoCirclesExercise, "tangentArcsToTwoCircles_hints.txt")
               ]
   }

makeRuleDescription :: String -> [String] -> [String] -> String
makeRuleDescription op is os
  | op == "circle" = "Draw a circle \'" ++ head os ++ "\' with radius \'" ++ is!!1 ++ "\' around point \'" ++ head is ++ "\'"
  | op == "circleCircleXn" = "Find the intersection points \'" ++ head os ++ "\' and \'" ++ os!!1 ++ "\' between circle \'"
    ++ head is ++ "\' and circle \'" ++ is!!1 ++ "\'"
  | op == "distance" = "Find the distance \'" ++ head os ++ "\' between point \'" ++ head is ++ "\' and point \'" ++ is!!1 ++ "\'"
  | op == "lineFromPoints" = "Construct a line \'" ++ head os ++ "\' between point \'" ++ head is ++ "\' and point \'" ++ is!!1 ++ "\'"
  | op == "lineLineXn" = "Find the intersection point \'" ++ head os ++ "\' between line " ++ head is ++ " and line \'" ++ is!!1 ++ "\'"
  | op == "lineCircleXn" = "Find the intersection points \'" ++ head os ++ "\' and \'" ++ os!!1 ++ "\' between line \'"
    ++ head is ++ "\' and circle \'" ++ is!!1 ++ "\'"
  | op == "explodeAngle" = "Make three points \'" ++ head os ++ "\', \'" ++ os!!1 ++ "\' and \'" ++ os!!2 ++
    "\' that define angle \'" ++ head is ++ "\'"
  | op == "concentricCircle" = "Draw a circle \'" ++ head os ++ "\' concentric to circle \'" ++ head is
    ++ "\' and at distance \'" ++ is!!1 ++ "\' away from it"
  | otherwise = ""

describeRule :: String -> (String, String)
describeRule r = (r, makeRuleDescription op is os)
  where
    parts = splitOn "--" r
    [op, il, ol] = parts
    is = splitOn "-" il
    os = splitOn "-" ol

describeRules :: [String] -> String
describeRules = concatMap ((\(x, y) -> "text " ++ T.unpack (T.toLower $ T.pack x) ++ " = {" ++ y ++ "}\n") . describeRule)

experimentMain :: IO ()
-- experimentMain = putStrLn . describeRules . getRuleIds $ square
experimentMain = defaultMain dr
