{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-} 
module Test.WorldTests where 

import Test.QuickCheck
import Test.QuickCheck.All
import Data.DeriveTH
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State

import Utils
import Terrain
import World
import Test.TerrainTests
import WorldGenerating
import AI
import Item

derive makeArbitrary ''CreatureType
derive makeArbitrary ''Material
derive makeArbitrary ''ItemType

instance Arbitrary World' where
    arbitrary = World <$> arbitrary <*> pure IM.empty <*> pure IM.empty <*> pure 0 <*> pure [] <*> pure IM.empty

arbitraryCreature :: Point -> Gen Creature'
arbitraryCreature p = Creature <$> arbitrary <*> arbitrary <*> pure (-1)
    <*> pure p <*> pure simpleAct <*> pure defaultAI <*> pure []

arbitraryItem :: Point -> Gen Item
arbitraryItem p = Item <$> pure (-1) <*> arbitrary <*> arbitrary <*> pure (ItemPos p)
    
worldWithCreatureAndItem :: Gen World'
worldWithCreatureAndItem = do
    w1 <- arbitrary
    pos <- validTerrainPoint (w1 ^. worldTerrain)
    c <- arbitraryCreature pos
    i <- arbitraryItem pos
    let w2 = addCreature c w1
        w3 = addItem i w2
    return w3

prop_itemPickUp = forAll worldWithCreatureAndItem $ \w ->
  let it = (w ^?! worldItems . traverse . itemId ) 
      cr = (w ^?! worldCreatures . traverse ) 
  in flip evalState w $ do
      cr2 <- pickUpItemId it cr
      it2 <- use (worldItems . at it) 
      let creatureItemCounter = (lengthOf (creatureItems . traverse) cr2) > 0 
      case it2 of
          Just it3 -> return ( creatureItemCounter && 
            ((it3 ^. itemState) == (ItemHeldBy (cr2 ^. creatureId))))
          Nothing -> return False

return []
worldTests = $quickCheckAll
