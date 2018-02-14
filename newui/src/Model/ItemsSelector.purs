module Model.ItemsSelector ( ItemSelector
                           , Option(..)
                           , Options(..)
                           , Consumption(..)
                           , Consumptions(..)
                           , emptySelector 
                           , freshSelector
                           , consumeItems
                           , selectItem
                           , changeLevelSelection
                           , changeModeSelection
                           , selectableBuildings
                           , numberConsumed
                           , isSelected
                           , availableLevels
                           , getBuildingOption
                           , currentlySelected) where

import Prelude (class Eq, class Show, show, map, (<>), (+), ($), (<<<), (>>=), (==), (<), (>), (>=))

import Data.Foldable (foldl)
import Data.List as List
import Data.List ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Tuple
import Partial.Unsafe (unsafePartial)

import Model.CoreTypes (Level(..), minimumSize, PlacedItem)
import Model.TownHallDefinitions (TownHallDefinition(..), AllowedBuilding(..), Mode(..), wallsToAllowedBuilding)


-- Types

type ItemSelector =
  { items :: List.List AllowedBuilding
  , selected :: Maybe String
  , options :: Options
  , consumptions :: Consumptions
  }

data Option = Option
  { availableLevels :: List.List Level
  , disabledModes :: List.List String
  , lockedModes :: List.List String
  , level :: Level
  , mode :: Maybe String 
  }

newtype Options = Options (Map.Map String Option)

data Consumption = Consumption
  { numberPlaced :: Int
  , modesUsed :: Map.Map String Int
  }

newtype Consumptions = Consumptions (Map.Map String Consumption)


-- Typeclasses

instance showOption :: Show Option where
  show (Option option) = show option.level <> "(" <> show option.mode <> ")"

derive instance eqOption :: Eq Option


instance showOptions :: Show Options where
  show (Options options) = show options

derive instance eqOptions :: Eq Options


instance showConsumption :: Show Consumption where
  show (Consumption c) = show c.numberPlaced

derive instance eqConsumption :: Eq Consumption


instance showConsumptions :: Show Consumptions where
  show (Consumptions cs) = show cs

derive instance eqConsumptions :: Eq Consumptions  


-- Construction functions 

emptySelector :: ItemSelector
emptySelector = 
  { items: List.Nil
  , selected: Nothing
  , options: Options Map.empty
  , consumptions: Consumptions Map.empty
  }

freshSelector :: TownHallDefinition -> ItemSelector
freshSelector (TownHallDefinition { level: level
                                  , defences: defences
                                  , army: army
                                  , resources: resources
                                  , traps: traps
                                  , walls: walls
                                  }) = 
  let
    buildings = defences <> army <> resources <> traps
    wallsAsBuildings = map wallsToAllowedBuilding walls
    all = buildings <> wallsAsBuildings
    options = Map.fromFoldable $ (map optionForAllowedBuilding all)
  in
    { items: all
    , selected: Nothing
    , options: Options options
    , consumptions: Consumptions Map.empty
    }

optionForAllowedBuilding :: AllowedBuilding -> Tuple String Option
optionForAllowedBuilding (AllowedBuilding { id: id
                                          , maxLevel: maxLevel
                                          , minLevel: minLevel
                                          , modes: modes
                                          }) = 
  let
    levels = 
      levelsList maxLevel minLevel

    option = Option { availableLevels: levels
                    , disabledModes: List.Nil
                    , lockedModes: List.Nil
                    , level: fromMaybe (Level 1) $ List.head levels
                    , mode: map (\(Mode { id: id' }) -> id') $ List.head modes
                    }
  in
    Tuple id option

levelsList :: Level -> Maybe Level -> List.List Level   
levelsList max maybeMin =
  let
     (Level min) = fromMaybe (Level 1) maybeMin

     (Level maxVal) = max
   in
     List.reverse $ map Level $ List.range min maxVal


-- Update selector to consume items

consumeItems :: List.List PlacedItem -> ItemSelector -> ItemSelector
consumeItems placed selector = 
  let
    consumptions = 
      placedItemsToConsumptions placed

    options =
      updateOptionsFromConsumptions consumptions selector.options selector.items 

    selected =
      selector.selected >>= (stillSelected selector.items consumptions)
  in   
    { items: selector.items
    , selected: selected
    , options: options
    , consumptions: consumptions
    }

-- Consumption handling

placedItemsToConsumptions :: List.List PlacedItem -> Consumptions
placedItemsToConsumptions items =
  Consumptions $ foldl updateConsumption Map.empty items

updateConsumption :: Map.Map String Consumption -> PlacedItem -> Map.Map String Consumption
updateConsumption result item =
    Map.alter (updateConsumptionWithItem item) item.id result 

updateConsumptionWithItem :: PlacedItem -> Maybe Consumption -> Maybe Consumption
updateConsumptionWithItem item consumption =
  case consumption of
    Nothing ->
      Just (newConsumption item)
    Just c ->
      Just (applyAdditionalConsumption item c)

newConsumption :: PlacedItem -> Consumption
newConsumption item =
  let
    modesUsed = (fromMaybe Map.empty <<< map singleConsumption) $ item.mode 
  in
    Consumption { numberPlaced: 1
                , modesUsed: modesUsed
                }
  where
    singleConsumption mode = Map.singleton mode 1

applyAdditionalConsumption :: PlacedItem -> Consumption -> Consumption
applyAdditionalConsumption item (Consumption { numberPlaced: numberPlaced
                                             , modesUsed: modesUsed}) =
  let
    newModesUsed = updateModesUsed modesUsed item.mode
  in
    Consumption { numberPlaced: numberPlaced + 1
                , modesUsed: newModesUsed
                }

updateModesUsed :: Map.Map String Int -> Maybe String -> Map.Map String Int
updateModesUsed used =
  fromMaybe used <<< map updateMode
  where
    updateMode m = Map.alter incrementCount m used
    incrementCount = Just <<< fromMaybe 1 <<< map (\i -> i + 1)

-- Option handling

updateOptionsFromConsumptions :: Consumptions -> Options -> List.List AllowedBuilding -> Options
updateOptionsFromConsumptions (Consumptions consumptions) (Options options) =
  Options <<< Map.fromFoldable <<< map (updateOptionsForItem consumptions options)

updateOptionsForItem :: Map.Map String Consumption -> Map.Map String Option -> AllowedBuilding -> Tuple String Option
updateOptionsForItem consumptions options (AllowedBuilding { id: id
                                                           , modes: modes }) =
  let
    option = unsafePartial $ fromJust $ Map.lookup id $ options

    consumption = Map.lookup id consumptions
  in
    (Tuple id <<< fromMaybe option <<< map (applyOptionUpdate modes option)) $ consumption 

applyOptionUpdate :: List.List Mode -> Option -> Consumption -> Option
applyOptionUpdate modes 
                  (Option option)
                  (Consumption { modesUsed: modesUsed }) = 
  let
    consumedModes =
      (map modeId <<< List.filter (isModeConsumed modesUsed)) $ modes 
      
    invalidMode = 
      (fromMaybe false <<< map (consumedBy consumedModes)) $ option.mode

    defaultMode =
      (map modeId <<< List.head) $ modes
  in
    Option (option { disabledModes = consumedModes 
                   , mode = if invalidMode then defaultMode else option.mode 
                   })
  where
    modeId (Mode { id: id' }) = id'
    consumedBy cm m = List.elem m cm

isModeConsumed :: Map.Map String Int -> Mode -> Boolean  
isModeConsumed modesUsed (Mode { id: id
                               , maxAllowed: maxAllowed }) =
  let
    numberPlaced =
      (fromMaybe 0 <<< Map.lookup id) $ modesUsed
  in
    (fromMaybe false <<< map (allUsed numberPlaced)) $ maxAllowed
  where
    allUsed n max = n >= max


-- Selected item handling

stillSelected :: List.List AllowedBuilding -> Consumptions -> String -> Maybe String
stillSelected buildings (Consumptions consumptions) id =
  let
    consumedCount = 
      numConsumed (Map.lookup id consumptions)

    allowedCount = 
      numberAllowed (findBuilding id buildings)
  in 
    if consumedCount < allowedCount then Just id else Nothing
  where
    numConsumed (Just (Consumption { numberPlaced: numberPlaced })) = numberPlaced 
    numConsumed Nothing = 0

    numberAllowed (Just (AllowedBuilding { quantity: quantity})) = quantity
    numberAllowed Nothing = 0



-- Updates to the model based on selection or changes or values

selectItem :: String -> ItemSelector -> ItemSelector
selectItem id selector =
  selector { selected = Just id }

changeLevelSelection :: String -> Level -> ItemSelector -> ItemSelector
changeLevelSelection id level selector =
  let
    modes =
      (fromMaybe List.Nil <<< map buildingModes <<< findBuilding id) $ selector.items 

    (Options options) =
      selector.options

    newOptions =
      Map.update (updateOptionLevel modes level) id options
  in
    selector { options = (Options newOptions) }
  where
    buildingModes (AllowedBuilding { modes: modes' }) = modes'

updateOptionLevel :: List.List Mode -> Level -> Option -> Maybe Option  
updateOptionLevel modes newLevel (Option option) =
  let
    lockedModes =
      identifyLockedModes newLevel modes

    newMode = 
      option.mode >>= (determineMode modes lockedModes)
  in
    Just (Option (option { level = newLevel
                         , mode = newMode
                         , lockedModes = lockedModes }))

determineMode :: List.List Mode -> List.List String -> String -> Maybe String
determineMode modes lockedModes mode =
  if List.elem mode lockedModes
  then (map modeId <<< List.head) $ modes
  else Just mode
  where
    modeId (Mode { id: id }) = id

identifyLockedModes :: Level -> List.List Mode -> List.List String
identifyLockedModes level modes = 
  (map modeId <<< List.filter (levelBelowMin level)) $ modes
  where
    levelBelowMin level' (Mode { minLevel: minLevel }) =
      (fromMaybe false <<< map (\l -> l > level')) $ minLevel

    modeId (Mode { id: id }) = id

changeModeSelection :: String -> String -> ItemSelector -> ItemSelector
changeModeSelection id mode selector =
  let
    (Options options) =
      selector.options

    newOptions =
      Map.update (setMode mode) id options
  in 
    selector { options = (Options newOptions) }
  where
    setMode mode' (Option option) =
      Just (Option (option { mode = Just mode' }))


-- Utility functions

findBuilding :: String -> List.List AllowedBuilding -> Maybe AllowedBuilding
findBuilding id = 
  List.head <<< List.filter (isBuilding id)
  where
    isBuilding bId (AllowedBuilding { id: id' }) = bId == id'


-- Query functions

selectableBuildings :: ItemSelector -> List.List AllowedBuilding
selectableBuildings selector =
  List.filter (isNotConsumed selector) selector.items

isNotConsumed :: ItemSelector -> AllowedBuilding -> Boolean
isNotConsumed selector building =
  let
    (AllowedBuilding b) = building
  in 
    (numberConsumed selector building) < b.quantity

numberConsumed :: ItemSelector -> AllowedBuilding -> Int
numberConsumed selector building =
  let
    (Consumptions c) = selector.consumptions

    (AllowedBuilding b) = building
  in
    (fromMaybe 0 <<< map (\(Consumption { numberPlaced: n }) -> n) <<< Map.lookup b.id) $ c

isSelected :: ItemSelector -> AllowedBuilding -> Boolean
isSelected selector (AllowedBuilding { id: id }) =
  Just id == selector.selected

availableLevels :: ItemSelector -> AllowedBuilding -> List.List Level
availableLevels selector (AllowedBuilding { id: id }) =
  let
    (Options options) = selector.options
  in
    (fromMaybe (Level 1 : List.Nil) <<< 
     map (\(Option option) -> option.availableLevels) <<< 
     Map.lookup id) $ 
     options

getBuildingOption :: ItemSelector -> AllowedBuilding -> Option
getBuildingOption selector (AllowedBuilding { id: id }) =
  let
    (Options options) = selector.options
  in
    unsafePartial $ fromJust $ Map.lookup id $ options

currentlySelected :: ItemSelector -> Maybe PlacedItem
currentlySelected selector =
  let
    (Options options) = selector.options
  in 
    map toPlacedItem selector.selected
  where
    toPlacedItem id = { id: id
                      , level: (optionFor id).level
                      , mode: (optionFor id).mode
                      , size: itemSize id
                      }

    optionFor id = 
      let
        (Options options) = selector.options
        (Option option) = unsafePartial $ fromJust $ Map.lookup id $ options
      in
        option

    itemSize id =
      fromMaybe minimumSize $
      map (\(AllowedBuilding b) -> b.size) $
      List.find (\(AllowedBuilding b) -> b.id == id) $
      selector.items

