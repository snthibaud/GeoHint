module Strategies where
import Data.Graph
import DataTypes
import Ideas.Common.Context hiding (Context)
import Ideas.Common.Library hiding (Context)
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Combinators hiding (repeat)
import Rules


makeNode :: [LObject] -> Operation
  -> (Rule Program, OperationLabel, [OperationLabel])
makeNode spec o@(l, _, is, _, _)
  | l == 0 = (createSpecification l spec, l, [])
  | otherwise = (createRule o, l, is)

-- geoSynth :: Specification -> Specification -> [LObject] -> Program
-- geoSynth inputSpec outputSpec inputObjects = ...

-- This function only works if no renaming of labeled objects took
-- place.
-- For example: if a point is labeled X,
-- the label X cannot be used
-- for a different point later on.
programToStrategy :: Program -> String -> LabeledStrategy Program
programToStrategy (operations, inputObjects) name =
  label name $ dependencyGraph $ graphFromEdges $
    map (makeNode inputObjects) operations

programsToStrategy :: [Program] -> String
  -> LabeledStrategy Program
programsToStrategy programs name =
  label name $ choice (map (`programToStrategy` "") programs)
