module Model.ItemsSelector ( ItemSelector
                           , Option(..)
                           , Options(..)
                           , Consumption(..)
                           , Consumptions(..)
                           , emptySelector 
                           , freshSelector ) where

import Prelude (class Eq, class Show, show, map, (<>), (+), ($))

import Data.Foldable (foldl)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple

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
      selector.options
      --updateOptionsFromConsumptions pallette.items consumptions pallette.options

    selected =
      selector.selected
      -- case pallette.selected of
      --   Nothing -> 
      --     Nothing
      --   Just id ->
      --     pallette.items          
      --       |> List.filter (\i -> i.id == id)
      --       |> List.head
      --       |> Maybe.andThen (\i -> 
      --           if isNotConsumed consumptions i
      --           then Just id
      --           else Nothing
      --         )
  in   
    { items: selector.items
    , selected: selected
    , options: options
    , consumptions: consumptions
    }

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
    modesUsed = fromMaybe Map.empty $ map (\mode -> Map.singleton mode 1) item.mode 
  in
    Consumption { numberPlaced: 1
                , modesUsed: modesUsed
                }

applyAdditionalConsumption :: PlacedItem -> Consumption -> Consumption
applyAdditionalConsumption item (Consumption { numberPlaced: numberPlaced
                                             , modesUsed: modesUsed}) =
  let
    newModesUsed = updateModesUsed item.mode modesUsed
  in
    Consumption { numberPlaced: numberPlaced + 1
                , modesUsed: newModesUsed
                }

updateModesUsed :: Maybe String -> Map.Map String Int -> Map.Map String Int
updateModesUsed mode used =
  fromMaybe used $ map (\m -> Map.alter incrementCount m used) mode
  where
    incrementCount count = Just $ fromMaybe 1 $ map (\i -> i + 1) $ count
