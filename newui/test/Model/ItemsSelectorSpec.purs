module Model.ItemsSelectorSpec where

import Prelude (Unit, discard, map, ($))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))

import Model.CoreTypes (Level(..))
import Model.TownHallDefinitions (AllowedBuilding(..))
import Model.ItemsSelector (Consumptions(..), Options(..), Option(..), emptySelector, freshSelector)
import Model.TownHallDefinitionsSpec (th11Definition)

spec :: forall r. Spec r Unit
spec = 
  describe "The Items Selector" do
    describe "Creation" do
      it "can produce an empty selector" do
        let selector = emptySelector
        selector.items `shouldEqual` List.Nil
        selector.selected `shouldEqual` Nothing
        selector.options `shouldEqual` (Options Map.empty)
        selector.consumptions `shouldEqual` (Consumptions Map.empty)

      it "can produce a selector from a given town hall definition" do
        let selector = freshSelector th11Definition
        (List.length selector.items) `shouldEqual` 37
        selector.selected `shouldEqual` Nothing
        let (Options options) = selector.options
        (Map.size options) `shouldEqual` 37
        selector.consumptions `shouldEqual` (Consumptions Map.empty)

      it "should correctly represent allowed buildings in the selector" do
        let selector = freshSelector th11Definition
        case (List.head selector.items) of
          Just (AllowedBuilding { id: id } ) ->      
            id `shouldEqual` "cannon"
          Nothing ->
            fail "An allowed building should be returned"

      it "should correctly generate options for a building in the selector" do
        let selector = freshSelector th11Definition
        let (Options options) = selector.options
        case (Map.lookup "cannon" options) of
          Just (Option { availableLevels: availableLevels
                       , level: level }) -> do
            availableLevels `shouldEqual` levelsTo15
            level `shouldEqual` (Level 15)
          Nothing ->
            fail "An option should be returned"        

levelsTo15 :: List.List Level
levelsTo15 =  List.reverse $ map Level $ List.range 1 15
