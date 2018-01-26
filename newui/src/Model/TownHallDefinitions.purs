module Model.TownHallDefinitions where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?), (.??))
import Data.Either (Either)

import Model.CoreTypes (Level(..), Size)

newtype TownHallDefinition = TownHallDefinition
  { level :: Level
  , defences :: List AllowedBuilding
  , army :: List AllowedBuilding
  , resources :: List AllowedBuilding
  , traps :: List AllowedBuilding
  , troops :: List Troop
  , spells :: List Spell
  , walls :: List Walls
  }

newtype AllowedBuilding = AllowedBuilding
  { id :: String
  , name :: String
  , quantity :: Int
  , maxLevel :: Level
  , minLevel :: Maybe Level
  , size :: Size 
  , modes :: List Mode
  }

newtype Mode = Mode
  { id :: String
  , name :: String
  , maxAllowed :: Maybe Int 
  , minLevel :: Maybe Level
  }

newtype Troop = Troop
  { id :: String
  , name :: String
  , maxLevel :: Level 
  }

newtype Spell = Spell
  { id :: String
  , name :: String
  , maxLevel :: Level 
  }

newtype Walls = Walls
  { quantity :: Int
  , maxLevel :: Level
  }


-- Json Decoders

instance decodeJsonTownHallDefinition :: DecodeJson TownHallDefinition where
  decodeJson json = do
    obj <- decodeJson json
    level <- obj .? "level"
    defences <- obj .? "defences"
    army <- obj .? "army"
    resources <- obj .? "resources"
    traps <- obj .? "traps"
    troops <- obj .? "troops"
    spells <- obj .? "spells"
    walls <- obj .? "walls"
    pure $ TownHallDefinition
      { level: Level level
      , defences: defences
      , army: army
      , resources: resources
      , traps: traps
      , troops: troops
      , spells: spells
      , walls: walls 
      }

instance decodeJsonAllowedBuilding :: DecodeJson AllowedBuilding where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    quantity <- obj .? "qty"
    level <- obj .? "level"
    minLevel <- obj .?? "minLevel"
    size <- obj .? "size"
    modes <- obj .?? "modes"
    pure $ AllowedBuilding 
      { id: id_
      , name: name
      , quantity: quantity
      , maxLevel: Level level
      , minLevel: map Level minLevel
      , size: size
      , modes: fromMaybe Nil modes 
      }

instance decodeJsonMode :: DecodeJson Mode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    max_ <- obj .?? "max"
    level <- obj .?? "level"
    pure $ Mode { id: id_, name: name, maxAllowed: max_, minLevel: map Level level }

instance decodeJsonTroop :: DecodeJson Troop where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    level <- obj .? "level"
    pure $ Troop { id: id_, name: name, maxLevel: Level level }

instance decodeJsonSpell :: DecodeJson Spell where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    level <- obj .? "level"
    pure $ Spell { id: id_, name: name, maxLevel: Level level }

instance decodeJsonWalls :: DecodeJson Walls where
  decodeJson json = do
    obj <- decodeJson json
    quantity <- obj .? "qty"
    level <- obj .? "level"
    pure $ Walls { quantity: quantity, maxLevel: Level level }

decodeTownHallDefinition :: Json -> Either String TownHallDefinition
decodeTownHallDefinition = decodeJson

