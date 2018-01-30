module Model.ItemsSelector ( ItemSelector
                           , Option
                           , Options(..)
                           , Consumption
                           , Consumptions(..)
                           , emptySelector 
                           , freshSelector ) where

import Prelude (class Eq, class Show, show, (<>), map)

import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))

import Model.CoreTypes (Level)
import Model.TownHallDefinitions (TownHallDefinition(..), AllowedBuilding, wallsToAllowedBuilding)


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
--    options = Map.fromFoldable $ List.map (\i -> (i.id, itemToOption i)) all
  in
    { items: all
    , selected: Nothing
    , options: Options Map.empty
    , consumptions: Consumptions Map.empty
    }

-- itemToOption : PalletteItem -> PalletteOption
-- itemToOption item = 
--   let
--     availableLevels = 
--       levelsList item.maxLevel item.minLevel
--   in
--     { availableLevels = availableLevels
--     , disabledModes = []
--     , lockedModes = []
--     , level = List.head availableLevels |> Maybe.withDefault 1
--     , mode = List.head item.modes |> Maybe.map (\m -> m.id)
--     }

-- levelsList : Level -> Maybe Level -> List Level   
-- levelsList max maybeMin =
--   let
--      min = Maybe.withDefault 1 maybeMin
--    in
--      List.range min max |> List.reverse 
