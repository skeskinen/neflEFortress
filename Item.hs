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

type ItemId = Int

data ItemState = 
      ItemPos Point
    | ItemHeldBy Int
    deriving (Eq)

instance Show ItemState where
    show (ItemPos a) = show a
    show (ItemHeldBy a) = "held by " ++ show a 

data Item = Item {
      _itemId       :: ItemId
    , _itemType     :: ItemType
    , _itemMaterial :: Material
    , _itemState    :: ItemState
}

makeLenses ''Item
makePrisms ''ItemState

instance Show Item where 
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

itemDesc :: Item -> String
itemDesc item = 
       materialDesc (item ^. itemMaterial) ++ " "
    ++ itemTypeDesc (item ^. itemType)

