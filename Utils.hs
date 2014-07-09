module Utils where
import Control.Lens

type Point = (Int, Int, Int)
type Area = (Point, Point)

areaPoints :: Area -> [Point]
areaPoints ((x1, y1, z1), (x2, y2, z2)) = 
    [(x, y, z) | 
        x <- [x1..x2]
      , y <- [y1..y2]
      , z <- [z1..z2]]

data Dir = DUp | DLeft | DDown | DRight | DTop | DBottom
    deriving (Enum, Eq, Bounded, Show)

reverseDir :: Dir -> Dir
reverseDir DUp     = DDown
reverseDir DDown   = DUp
reverseDir DLeft   = DRight
reverseDir DRight  = DLeft
reverseDir DTop    = DBottom
reverseDir DBottom = DTop


addDir :: Dir -> Point -> Point
addDir DUp     = _2 -~ 1
addDir DDown   = _2 +~ 1
addDir DLeft   = _1 -~ 1
addDir DRight  = _1 +~ 1
addDir DTop    = _3 -~ 1
addDir DBottom = _3 +~ 1

dirsToPoints :: [Dir] -> Point -> [Point]
dirsToPoints dirs pos = foldl (\list@(prevPos : _) dir -> addDir dir prevPos : list) [pos] dirs

pointsToDirs :: [Point] -> Maybe [Dir]
pointsToDirs points = mapM getDir $ zip points (tail points)
  where
    getDir ((x1, y1, z1), (x2, y2, z2)) = lookup (x2-x1, y2-y1,z2-z1) $ 
        map (\dir -> (addDir dir (0,0,0), dir)) [minBound .. maxBound]

