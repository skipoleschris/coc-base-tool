module Model.ItemsSelectorSpec where

import Prelude (Unit, discard)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))

--import Model.CoreTypes (Level(..))
--import Model.TownHallDefinitions (TownHallDefinition(..), decodeTownHallDefinition)
import Model.ItemsSelector (Consumptions(..), Options(..), emptySelector, freshSelector)
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
