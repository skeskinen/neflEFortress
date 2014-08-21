{-# LANGUAGE TemplateHaskell #-}

module Building where

import Control.Lens
import Control.Monad.State

import Utils

type BuildingIdP world = ObjId (BuildingP world)

data BuildingType = 
    BuildingField
  | BuildingBrewery
  deriving Show

data BuildingInt = FieldTimer Int

data BuildingState = 
    BuildingBuilding Int 
  | BuildingReady
  deriving Show

data BuildingP world = Building {
    _buildingType     :: BuildingType
  , _buildingPos      :: Point
  , _buildingId       :: BuildingIdP world
  , _buildingState    :: BuildingState
  , _buildingInternal :: Maybe BuildingInt
  , _buildingAct      :: BuildingP world -> State world ()
} 

makePrisms ''BuildingState
makePrisms ''BuildingInt
makeLenses ''BuildingP

instance Show (BuildingP w) where
    show b = show (b ^. buildingType) ++ " " ++ show (b ^. buildingState)

defBuilding :: BuildingP w
defBuilding = Building {
      _buildingType = BuildingBrewery
    , _buildingPos = (0,0,0)
    , _buildingId = ObjId (-1)
    , _buildingState = BuildingBuilding 5
    , _buildingInternal = Nothing
    , _buildingAct = const (return ())
}


