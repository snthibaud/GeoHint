module Rules (createRule, createSpecification, getRuleIds)
where
import Control.Applicative
import Data.List (intercalate, sort, sortBy, sortOn)
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe)
import DataTypes
import GeoLibrary
import Ideas.Common.Id (describe)
import Ideas.Common.Rule.Abstract (Rule, makeRule)

objLineLineXn :: Refinement
objLineLineXn [LineObject l1, LineObject l2] = Just $ maybe []
  (replicate 1 . PointObject) (lineLineXn l1 l2)
objLineLineXn _ = Nothing

objLineCircleXn :: Refinement
objLineCircleXn [LineObject l, CircleObject c] =
  Just $ map PointObject (lineCircleXn l c)
objLineCircleXn _ = Nothing

objCircleCircleXn :: Refinement
objCircleCircleXn [CircleObject c1, CircleObject c2] =
  Just $ map PointObject (circleCircleXn c1 c2)
objCircleCircleXn _ = Nothing

objPerpendicularToLineThruPoint :: Refinement
objPerpendicularToLineThruPoint [LineObject l, PointObject p] =
  Just [LineObject $ perpendicularToLineThruPoint l p]
objPerpendicularToLineThruPoint _ = Nothing

objPerpendicularBisector2Points :: Refinement
objPerpendicularBisector2Points [PointObject p1, PointObject p2] =
  Just [LineObject $ perpendicularBisector2Points p1 p2]
objPerpendicularBisector2Points _ = Nothing

objDistance :: Refinement
objDistance [PointObject p1, PointObject p2] =
  Just [LengthObject $ distance p1 p2]
objDistance _ = Nothing

objLineFromPoints :: Refinement
objLineFromPoints [PointObject p1, PointObject p2] =
  Just [LineObject $ lineFromPoints p1 p2]
objLineFromPoints _ = Nothing

objCircle :: Refinement
objCircle [PointObject p, LengthObject d] =
  Just [CircleObject $ Circle p d]
objCircle _ = Nothing

objConcentricCircle :: Refinement
objConcentricCircle [CircleObject c, LengthObject o] =
  Just [CircleObject $ concentricCircle c o]

objExplodeAngle :: Refinement
objExplodeAngle [AngleObject a] =
  Just . map PointObject $ explodeAngle a
objExplodeAngle _ = Nothing

selectObjects :: [ObjectLabel] -> [LObject] -> Maybe [Object]
selectObjects (l:ls) e = do
    o <- lookup l e
    os <- selectObjects ls e
    return (o:os)
selectObjects [] e = Just []

execute :: Operator -> Refinement
execute o = fromMaybe (const Nothing) (lookup o [
    ("lineLineXn", objLineLineXn),
    ("lineCircleXn", objLineCircleXn),
    ("circleCircleXn", objCircleCircleXn),
    ("perpendicularToLineThruPoint",
      objPerpendicularToLineThruPoint),
    ("distance", objDistance),
    ("lineFromPoints", objLineFromPoints),
    ("circle", objCircle),
    ("perpendicularBisector2Points",
      objPerpendicularBisector2Points),
    ("explodeAngle", objExplodeAngle),
    ("concentricCircle", objConcentricCircle)
  ])

makeRuleId :: Operator -> [ObjectLabel] -> [ObjectLabel] -> String
makeRuleId op is os = op ++ "--" ++ intercalate "-" is
  ++ "--" ++ intercalate "-" os

getRuleIds :: Program -> [String]
getRuleIds (operations, _) =
  map (\(_, o, _, is, os) -> makeRuleId o is os) operations

createRule :: Operation -> Rule Program
createRule (l, op, ols, i, o) =
  makeRule (makeRuleId op i o) $ \(ops, lobjs) -> do
    inObjects <- selectObjects i lobjs
    outObjects <- execute op inObjects
    return ((l, op, ols, i, o):ops, zip o outObjects ++ lobjs)

createSpecification :: OperationLabel -> [LObject] -> Rule Program
createSpecification l lobjs =
  makeRule op $ \_ ->
    Just ([(l, op, [], [], map fst lobjs)], lobjs)
      where
        op = "specification"
