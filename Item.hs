{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Item where

import Control.Lens

import Utils

data Material =
      Wood
    | Iron
    | None
    deriving (Enum, Eq)

data ItemType =
      Bed
    | Rock
    | Wheat
    | Drink
    deriving (Enum, Eq)

data ItemState c = 
      ItemPos Point
    | ItemHeldBy (ObjId c)
    deriving (Eq)

instance Show (ItemState c) where
    show (ItemPos a) = show a
    show (ItemHeldBy a) = "held by " ++ show a 

type ItemIdP world c = ObjId (ItemP world c)

data ItemP world c = Item {
      _itemId       :: ItemIdP world c
    , _itemType     :: ItemType
    , _itemMaterial :: Material
    , _itemState    :: ItemState c
}

makeLenses ''ItemP
makePrisms ''ItemState

instance Show (ItemP world c) where 
    show item = itemDesc item ++ " " ++ show (item ^. itemState)

materialDesc :: Material -> String
materialDesc = desc
  where
    desc Wood = "wooden"
    desc Iron = "iron"
    desc None = ""

itemTypeDesc :: ItemType -> String
itemTypeDesc = desc
  where
    desc Bed  = "bed"
    desc Rock = "rock"
    desc Wheat = "wheat"
    desc Drink = "drink"

itemDesc :: ItemP world c -> String
itemDesc item = 
       materialDesc (item ^. itemMaterial) ++ " "
    ++ itemTypeDesc (item ^. itemType)

