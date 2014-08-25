module Utils where
import Control.Lens

type Point = (Int, Int, Int)
type Area = (Point, Point)

newtype ObjId a = ObjId { getObjId :: Int }

instance Show (ObjId a) where
    show = show . getObjId

instance Eq (ObjId a) where
    (ObjId a) == (ObjId b) = a == b

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

boundPoint :: Point -> ((Point, Point)) -> Point
boundPoint p (mi, ma) = p & _1 %~ f _1
                          & _2 %~ f _2
                          & _3 %~ f _3
  where
    f l = (max (mi ^. l)) . (min (ma ^. l - 1))

addDir :: Dir -> Point -> Point
addDir DUp     = addDirI DUp     1
addDir DDown   = addDirI DDown   1
addDir DLeft   = addDirI DLeft   1
addDir DRight  = addDirI DRight  1
addDir DTop    = addDirI DTop    1
addDir DBottom = addDirI DBottom 1

addDirI :: Dir -> Int -> Point -> Point
addDirI DUp     i = _2 +~ i
addDirI DDown   i = _2 -~ i
addDirI DLeft   i = _1 -~ i
addDirI DRight  i = _1 +~ i
addDirI DTop    i = _3 +~ i
addDirI DBottom i = _3 -~ i

dirsToPoints :: [Dir] -> Point -> [Point]
dirsToPoints dirs pos = foldl (\list@(prevPos : _) dir -> addDir dir prevPos : list) [pos] dirs

pointsToDirs :: [Point] -> Maybe [Dir]
pointsToDirs points = mapM getDir $ zip points (tail points)
  where
    getDir ((x1, y1, z1), (x2, y2, z2)) = lookup (x2-x1, y2-y1,z2-z1) $ 
        map (\dir -> (addDir dir (0,0,0), dir)) [minBound .. maxBound]

