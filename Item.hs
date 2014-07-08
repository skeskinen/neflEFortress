{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Item where

import Control.Lens

import Utils

data Material =
      Wood
    | Iron
    deriving (Enum, Eq)

data ItemType =
      Bed
    | Rock
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

itemTypeDesc :: ItemType -> String
itemTypeDesc = desc
  where
    desc Bed  = "bed"
    desc Rock = "rock"

itemDesc :: Item -> String
itemDesc item = 
       materialDesc (item ^. itemMaterial) ++ " "
    ++ itemTypeDesc (item ^. itemType)

