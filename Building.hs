{-# LANGUAGE TemplateHaskell #-}

module Building where

import Control.Lens
import Control.Monad.State

import Utils

type BuildingId = Int

data BuildingType = 
    BuildingField
  | BuildingBrewery
  deriving Show

data BuildingInt = FieldTimer Int

data BuildingState = 
    BuildingBuilding Int 
  | BuildingReady
  deriving Show

data Building world = Building {
    _buildingType     :: BuildingType
  , _buildingPos      :: Point
  , _buildingId       :: BuildingId
  , _buildingState    :: BuildingState
  , _buildingInternal :: Maybe BuildingInt
  , _buildingAct      :: Building world -> State world ()
} 

makePrisms ''BuildingState
makePrisms ''BuildingInt
makeLenses ''Building

instance Show (Building w) where
    show b = show (b ^. buildingType) ++ " " ++ show (b ^. buildingState)

defBuilding :: Building w
defBuilding = Building {
      _buildingType = BuildingBrewery
    , _buildingPos = (0,0,0)
    , _buildingId = -1
    , _buildingState = BuildingBuilding 5
    , _buildingInternal = Nothing
    , _buildingAct = const (return ())
}


