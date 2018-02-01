module Model.ItemsSelector ( ItemSelector
                           , Option(..)
                           , Options(..)
                           , Consumption(..)
                           , Consumptions(..)
                           , emptySelector 
                           , freshSelector
                           , consumeItems
                           , selectItem ) where

import Prelude (class Eq, class Show, show, map, (<>), (+), ($), (<<<), (>>=), (==), (<), (>=))

import Data.Foldable (foldl)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Tuple
import Partial.Unsafe (unsafePartial)

import Model.CoreTypes (Level(..), PlacedItem)
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
    availableLevels = 
      levelsList maxLevel minLevel

    option = Option { availableLevels: availableLevels
                    , disabledModes: List.Nil
                    , lockedModes: List.Nil
                    , level: fromMaybe (Level 1) $ List.head availableLevels
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
                  (Option { availableLevels: availableLevels
                          , lockedModes: lockedModes
                          , level: level
                          , mode: mode })
                  (Consumption { modesUsed: modesUsed }) = 
  let
    consumedModes =
      (map modeId <<< List.filter (isModeConsumed modesUsed)) $ modes 
      
    invalidMode = 
      (fromMaybe false <<< map (consumedBy consumedModes)) $ mode

    defaultMode =
      (map modeId <<< List.head) $ modes
  in
    Option { availableLevels: availableLevels
           , disabledModes: consumedModes 
           , lockedModes: lockedModes
           , level: level 
           , mode: if invalidMode then defaultMode else mode 
           }
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
      numberConsumed (Map.lookup id consumptions)

    allowedCount = 
      numberAllowed ((List.head <<< List.filter (isBuilding id)) $ buildings)
  in 
    if consumedCount < allowedCount then Just id else Nothing
  where
    numberConsumed (Just (Consumption { numberPlaced: numberPlaced })) = numberPlaced 
    numberConsumed Nothing = 0

    isBuilding bId (AllowedBuilding { id: id' }) = bId == id'

    numberAllowed (Just (AllowedBuilding { quantity: quantity})) = quantity
    numberAllowed Nothing = 0



-- Updates to the model based on selection or changes or values

selectItem :: String -> ItemSelector -> ItemSelector
selectItem id selector =
  selector { selected = Just id }
