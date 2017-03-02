module GeoGebraParser (parseFile) where
import DataTypes
import Text.XML.Light
import Data.Maybe

data Command = Line | Circle | Intersect | OrthogonalLine

parseFile :: FilePath -> IO ()
parseFile f = do
  fData <- readFile f
  print . load $ fData

getRootElement :: String -> Element
getRootElement s = case parseXMLDoc s of
  (Just e) -> e
  _ -> error "Couldn't parse given document."

makeQName :: String -> QName
makeQName s = QName s Nothing Nothing

getAttr :: Element -> String -> String
getAttr e s = fromJust $ findAttr (makeQName s) e

isPoint :: Element -> Bool
isPoint e = (qName (elName e) == "element") && (getAttr e "type" == "point")

load :: String -> [Point]
load s = do
  construction <- [fromJust $ findChild (makeQName "construction") (getRootElement s)]
  point <- filterChildren isPoint construction
  coordinate <- [fromJust $ findChild (makeQName "coords") point]
  return $ Point (read $ getAttr coordinate "x") (read $ getAttr coordinate "y")
