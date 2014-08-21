{-# LANGUAGE TemplateHaskell #-}
module Creature where

import Control.Monad.State
import Control.Lens

import Item
import Utils

import AIState

type CreatureIdP world = ObjId (CreatureP world)

nameGenerator :: String
nameGenerator = "Uther"

data CreatureType = 
    CreatureNefle
  deriving (Enum, Eq)

data CreatureP world = Creature {
      _creatureName  :: String
    , _creatureType  :: CreatureType
    , _creatureId    :: CreatureIdP world 
    , _creaturePos   :: (Int, Int, Int)
    , _creatureAct   :: CreatureP world -> State world ()
    , _creatureState :: AIState world (CreatureP world)
    , _creatureItems :: [ItemP world (CreatureP world)]
}

makeLenses ''CreatureP

instance Show CreatureType where
    show CreatureNefle = "Nefle"

instance Show (CreatureP w) where
    show c = (c ^. creatureName) ++ "; " ++ show (c ^. creatureType) ++ "; " 
      ++ show (c ^. creaturePos) 
      ++ "; Carried items: " ++ show (lengthOf (creatureItems . traverse) c) ++ ", " 
      ++ show (c ^. creatureItems) ++ "; " ++ show (c ^. creatureState)


