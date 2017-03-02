module DataTypes where

data Angle = Angle Double deriving (Read, Show)
data Vector = Vector Double Double deriving (Read, Show)
data Point = Point Double Double deriving (Read, Show)
data Line = Line Point Vector deriving (Read, Show)
data Circle = Circle Point Double deriving (Read, Show)
type Length = Double

data Object = AngleObject Angle | PointObject Point
  | LineObject Line | CircleObject Circle
  | LengthObject Length deriving (Read, Show)
type Refinement = [Object] -> Maybe [Object]
type ObjectLabel = String
type OperationLabel = Int
type LObject = (ObjectLabel, Object)
type Operator = String
type Operation = (OperationLabel, Operator, [OperationLabel],
                  [ObjectLabel], [ObjectLabel])
type Program = ([Operation], [LObject])
