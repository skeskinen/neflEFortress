{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Item where

import Control.Lens

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
      ItemPos (Int, Int, Int)
    | ItemHeldBy Int

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

instance Show Item where 
    show item = itemDesc item ++ " " ++ show (item ^. itemState)

_ItemPos :: Prism' ItemState (Int, Int, Int) 
_ItemPos = prism' ItemPos f
  where
    f (ItemPos a) = Just a
    f _ = Nothing

_ItemHeldBy :: Prism' ItemState Int 
_ItemHeldBy = prism' ItemHeldBy f
  where
    f (ItemHeldBy a) = Just a
    f _ = Nothing

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

