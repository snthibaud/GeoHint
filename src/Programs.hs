module Programs where
import DataTypes
import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (fromJust)

type Statement = (Operator, [ObjectLabel], [ObjectLabel])
type LabeledStatement = (OperationLabel, Statement)

numberStatements :: [Statement] -> [LabeledStatement]
numberStatements = zip [0..]

getDefinitions :: [LabeledStatement]
  -> Map.Map ObjectLabel OperationLabel
getDefinitions [] = Map.empty
getDefinitions ((i, (op, is, os)):xs) = Map.union
  (Map.fromList (zip os (repeat i))) (getDefinitions xs)

createOperation ::
  (LabeledStatement, Map.Map ObjectLabel OperationLabel)
  -> Operation
createOperation ((i, (op, is, os)), m) = (i, op, nub $
  map (fromJust . flip Map.lookup m) is, is, os)

createOperations :: [Statement] -> [Operation]
createOperations stmts = map createOperation statementsWithDefs
  where
    labeledStatements = numberStatements stmts
    definitions = [getDefinitions (take i labeledStatements)|
                   i <- [0..length labeledStatements]]
    statementsWithDefs = zip labeledStatements definitions

createProgram :: [LObject] -> [Statement] -> Program
createProgram spec stmts = (
  createOperations (("specification", [], map fst spec):stmts),
  spec
  )

square :: Program
square = createProgram
    [ ("a", PointObject $ Point 0 0)
    , ("b", PointObject $ Point 300 0)
    ]
    -- Elements above are given
    [ ("distance", ["a","b"], ["r1"])
    , ("circle", ["a", "r1"], ["x"])
    , ("circle", ["b", "r1"], ["y"])
    , ("circleCircleXn", ["x", "y"], ["c", "d"])
    , ("lineFromPoints", ["c", "d"], ["l1"])
    , ("lineFromPoints", ["a", "b"], ["l2"])
    , ("lineLineXn", ["l1", "l2"], ["e"])
    , ("distance", ["a", "e"], ["r2"])
    , ("circle", ["e", "r2"], ["z"])
    , ("lineCircleXn", ["l1", "z"], ["f", "g"])
    , ("lineFromPoints", ["a", "f"], ["l3"])
    , ("lineCircleXn", ["l3", "x"], ["k", "i"])
    , ("lineFromPoints", ["a", "g"], ["l4"])
    , ("lineCircleXn", ["l4", "x"], ["h", "j"])
    , ("lineFromPoints", ["i", "j"], ["l5"])
    , ("lineFromPoints", ["j", "k"], ["l6"])
    , ("lineFromPoints", ["k", "h"], ["l7"])
    , ("lineFromPoints", ["h", "i"], ["l8"])
    ]

hexagon :: Program
hexagon = createProgram
  [ ("m", PointObject $ Point 0 0)
  , ("p", PointObject $ Point 145 66)
  , ("r", LengthObject 267)
  ]
  [ ("circle", ["m", "r"], ["X"])
  -- Elements above are given
  , ("lineFromPoints", ["m", "p"], ["l"])
  , ("lineCircleXn", ["l", "X"], ["a", "b"])
  , ("circle", ["a", "r"], ["Y"])
  , ("circle", ["b", "r"], ["Z"])
  , ("circleCircleXn", ["X", "Y"], ["c", "d"])
  , ("circleCircleXn", ["X", "Z"], ["e", "f"])
  , ("lineFromPoints", ["c", "a"], ["lh1"])
  , ("lineFromPoints", ["a", "d"], ["lh2"])
  , ("lineFromPoints", ["d", "e"], ["lh3"])
  , ("lineFromPoints", ["e", "b"], ["lh4"])
  , ("lineFromPoints", ["b", "f"], ["lh5"])
  , ("lineFromPoints", ["f", "c"], ["lh6"])
  ]

triangleGiven2SidesAndIncludedAngle :: Program
triangleGiven2SidesAndIncludedAngle = createProgram
  [ ("alpha", AngleObject $ Angle 1.95809317826)
  , ("d1", LengthObject 828)
  , ("d2", LengthObject 631)
  ]
  -- Elements above are given
  [ ("explodeAngle", ["alpha"], ["b", "a", "c"])
  , ("lineFromPoints", ["a", "b"], ["lab"])
  , ("lineFromPoints", ["a", "c"], ["lac"])
  , ("circle", ["a", "d1"], ["X"])
  , ("circle", ["a", "d2"], ["Y"])
  , ("lineCircleXn", ["lab", "X"], ["d", "f"])
  , ("lineCircleXn", ["lac", "Y"], ["e", "g"])
  , ("lineFromPoints", ["d", "e"], ["lde"])
  ]

tangentArcsToTwoCircles :: Program
tangentArcsToTwoCircles = createProgram
  [ ("a", PointObject $ Point 0 0)
  , ("b", PointObject $ Point 507 157)
  , ("r1", LengthObject 629)
  , ("r2", LengthObject 142)
  , ("ra", LengthObject 817)
  ]
  [ ("circle", ["a", "r1"], ["c1"])
  , ("circle", ["b", "r2"], ["c2"])
  -- Elements above are given
  , ("concentricCircle", ["c1", "ra"], ["cc1"])
  , ("concentricCircle", ["c2", "ra"], ["cc2"])
  , ("circleCircleXn", ["cc1", "cc2"], ["c", "d"])
  , ("circle", ["c", "ra"], ["a1"])
  , ("circle", ["d", "ra"], ["a2"])
  ]
