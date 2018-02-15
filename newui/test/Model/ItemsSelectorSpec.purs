module Model.ItemsSelectorSpec where

import Prelude (Unit, discard, map, (==), ($))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Data.List as List
import Data.List ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

import Model.CoreTypes (Level(..), Size(..), PlacedItem)
import Model.TownHallDefinitions (AllowedBuilding(..))
import Model.ItemsSelector
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

      it "should consume the given items" do
        let selector = freshSelector th11Definition
        let placedItems = { id: "eagle-artillery", level: Level 2, mode: Nothing, size: sizeOf 4 } :
                          { id: "cannon", level: Level 15, mode: Just "burst", size: sizeOf 3 } :
                          { id: "cannon", level: Level 14, mode: Just "normal", size: sizeOf 3 } :
                          { id: "cannon", level: Level 14, mode: Just "normal", size: sizeOf 3 } :
                          List.Nil
        let result = consumeItems placedItems selector
        let (Consumptions consumptions) = result.consumptions

        case (Map.lookup "eagle-artillery" consumptions) of
          Just (Consumption { numberPlaced: numberPlaced
                            , modesUsed: modesUsed }) -> do
            numberPlaced `shouldEqual` 1
            (Map.isEmpty modesUsed) `shouldEqual` true
          Nothing ->
            fail "A consumption should be returned"

        case (Map.lookup "cannon" consumptions) of
          Just (Consumption { numberPlaced: numberPlaced
                            , modesUsed: modesUsed }) -> do
            numberPlaced `shouldEqual` 3
            (Map.lookup "normal" modesUsed) `shouldEqual` Just 2
            (Map.lookup "burst" modesUsed) `shouldEqual` Just 1
          Nothing ->
            fail "A consumption should be returned"

      it "should retain the selected item if not all have been consumed" do
        let selector = selectItem "cannon" $ freshSelector th11Definition
        let placedItems = { id: "eagle-artillery", level: Level 2, mode: Nothing, size: sizeOf 4 } :
                          { id: "cannon", level: Level 15, mode: Just "burst", size: sizeOf 3 } :
                          { id: "cannon", level: Level 14, mode: Just "normal", size: sizeOf 3 } :
                          { id: "cannon", level: Level 14, mode: Just "normal", size: sizeOf 3 } :
                          List.Nil
        let result = consumeItems placedItems selector
        result.selected `shouldEqual` (Just "cannon")

      it "should deselect the selected item if all have been consumed" do
        let selector = selectItem "eagle-artillery" $ freshSelector th11Definition
        let placedItems = { id: "eagle-artillery", level: Level 2, mode: Nothing, size: sizeOf 4 } :
                          { id: "cannon", level: Level 15, mode: Just "burst", size: sizeOf 3 } :
                          { id: "cannon", level: Level 14, mode: Just "normal", size: sizeOf 3 } :
                          { id: "cannon", level: Level 14, mode: Just "normal", size: sizeOf 3 } :
                          List.Nil
        let result = consumeItems placedItems selector
        result.selected `shouldEqual` Nothing

      it "should disable modes where all allowed have been placed" do
        let selector = selectItem "cannon" $ freshSelector th11Definition
        let placedItems = { id: "cannon", level: Level 15, mode: Just "burst", size: sizeOf 3 } :
                          List.Nil
        let result = consumeItems placedItems selector
        let (Options options) = result.options

        case (Map.lookup "cannon" options) of
          Just (Option { disabledModes: disabledModes
                       , mode: mode }) -> do
            disabledModes `shouldEqual` ("burst" : List.Nil)
            mode `shouldEqual` Just "normal"
          Nothing ->
            fail "An option should be returned"

      it "should allow the selected level of an item to be changed" do
        let selector = freshSelector th11Definition
        let (Options options) = selector.options
        case (Map.lookup "cannon" options) of
          Just (Option { level: level }) -> do
            level `shouldEqual` Level 15
          Nothing ->
            fail "An option should be returned"

        let selector' = changeLevelSelection "cannon" (Level 12) selector
        let (Options options') = selector'.options
        case (Map.lookup "cannon" options') of
          Just (Option option) -> do
            option.level `shouldEqual` Level 12
            option.mode `shouldEqual` Just "normal"
            option.lockedModes `shouldEqual` List.Nil
          Nothing ->
            fail "An option should be returned"

      it "should lock and deselect any no loger accessible nodes when the selected level of an item is changed" do
        let selector = changeLevelSelection "cannon" (Level 3) $
                       changeModeSelection "cannon" "burst" $ 
                       freshSelector th11Definition
        let (Options options) = selector.options
        case (Map.lookup "cannon" options) of
          Just (Option option) -> do
            option.level `shouldEqual` Level 3
            option.mode `shouldEqual` Just "normal"
            option.lockedModes `shouldEqual` ("burst" : List.Nil)
          Nothing ->
            fail "An option should be returned"

      it "should allow the selected mode of an item to be changed" do
        let selector = freshSelector th11Definition
        let (Options options) = selector.options
        case (Map.lookup "cannon" options) of
          Just (Option { mode: mode }) -> do
            mode `shouldEqual` Just "normal"
          Nothing ->
            fail "An option should be returned"

        let selector' = changeModeSelection "cannon" "burst" selector
        let (Options options') = selector'.options
        case (Map.lookup "cannon" options') of
          Just (Option { mode: mode' }) -> do
            mode' `shouldEqual` Just "burst"
          Nothing ->
            fail "An option should be returned"

      it "should provide a list of only those buildings not consumed" do
        let placedItems = townHall :
                          mortar :
                          mortar :
                          mortar :
                          mortar :
                          List.Nil
        let ids = map (\(AllowedBuilding b) -> b.id) $
                  selectableBuildings $
                  consumeItems placedItems $ 
                  freshSelector $ 
                  th11Definition
        List.elem "town-hall" ids `shouldEqual` false
        List.elem "mortar" ids `shouldEqual` false
        List.elem "cannon" ids `shouldEqual` true

      it "should return info for a Town Hall" do
        let selector = freshSelector $ 
                       th11Definition
        let townHallB = findBuilding "town-hall" selector
        let info = buildingInfo townHallB selector

        info.id `shouldEqual` "town-hall"
        info.name `shouldEqual` "Town Hall"
        info.quantity `shouldEqual` 1
        info.levels `shouldEqual` (Level 11 : List.Nil)
        info.modes `shouldEqual` List.Nil
        info.isSelected `shouldEqual` false
        info.placedCount `shouldEqual` 0
        info.level `shouldEqual` (Level 11)
        info.mode `shouldEqual` Nothing

      it "should return info for a Cannon" do
        let selector = freshSelector $ 
                       th11Definition
        let cannonB = findBuilding "cannon" selector
        let (AllowedBuilding b) = cannonB
        let info = buildingInfo cannonB selector

        info.id `shouldEqual` "cannon"
        info.name `shouldEqual` "Cannon"
        info.quantity `shouldEqual` 7
        info.levels `shouldEqual` levelsTo15
        info.modes `shouldEqual` b.modes
        info.isSelected `shouldEqual` false
        (info.isModeDisabled "normal") `shouldEqual` false
        (info.isModeDisabled "burst") `shouldEqual` false
        info.placedCount `shouldEqual` 0
        info.level `shouldEqual` (Level 15)
        info.mode `shouldEqual` (Just "normal")

      it "should indicate that a building is the selected one" do
        let selector = selectItem "cannon" $
                       freshSelector $ 
                       th11Definition
        let cannonB = findBuilding "cannon" selector
        let info = buildingInfo cannonB selector

        info.isSelected `shouldEqual` true

      it "should return the correct number of placed instanced of a building" do
        let placedItems = mortar :
                          mortar :
                          mortar :
                          List.Nil
        let selector = consumeItems placedItems $
                       freshSelector $ 
                       th11Definition
        let mortarB = findBuilding "mortar" selector
        let info = buildingInfo mortarB selector

        info.placedCount `shouldEqual` 3

      it "should return correct level and mode when these have been changed" do
        let selector = changeModeSelection "cannon" "burst" $
                       changeLevelSelection "cannon" (Level 13) $
                       freshSelector $ 
                       th11Definition
        let cannonB = findBuilding "cannon" selector
        let info = buildingInfo cannonB selector

        info.level `shouldEqual` (Level 13)
        info.mode `shouldEqual` (Just "burst")

      it "should disable a mode that is not available at the current building level" do
        let selector = changeLevelSelection "cannon" (Level 4) $
                       freshSelector $ 
                       th11Definition
        let cannonB = findBuilding "cannon" selector
        let info = buildingInfo cannonB selector

        (info.isModeDisabled "normal") `shouldEqual` false
        (info.isModeDisabled "burst") `shouldEqual` true

      it "should disable a mode where the allowed number has been placed" do
        let placedItems = burstCannon :
                          List.Nil
        let selector = consumeItems placedItems $
                       freshSelector $ 
                       th11Definition
        let cannonB = findBuilding "cannon" selector
        let info = buildingInfo cannonB selector

        (info.isModeDisabled "normal") `shouldEqual` false
        (info.isModeDisabled "burst") `shouldEqual` true

findBuilding :: String -> ItemSelector -> AllowedBuilding
findBuilding id selector =
  unsafePartial $ fromJust $ List.find (\(AllowedBuilding b) -> b.id == id) $ selector.items

levelsTo15 :: List.List Level
levelsTo15 =  List.reverse $ map Level $ List.range 1 15

sizeOf :: Int -> Size
sizeOf s = Size { width: s, height: s } 

townHall :: PlacedItem
townHall = { id: "town-hall"
           , level: Level 11
           , mode: Nothing
           , size: sizeOf 4 
           }

mortar :: PlacedItem
mortar = { id: "mortar"
         , level: Level 9
         , mode: Nothing
         , size: sizeOf 3 
         }
           
burstCannon :: PlacedItem
burstCannon = { id: "cannon"
              , level: Level 15
              , mode: Just "burst"
              , size: sizeOf 3 
              }
