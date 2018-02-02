module Model.CoreTypes where

import Prelude (class Eq, class Show, bind, pure, show, ($), (==))

import Data.Ord
import Data.Maybe (Maybe)

import Data.Argonaut (class DecodeJson, decodeJson, (.?))

-- Define the Level type for different building and troop levels

newtype Level = Level Int

instance showLevel :: Show Level where
  show (Level lvl) = show lvl

instance eqLevel :: Eq Level where
  eq (Level left) (Level right) = left == right

instance ordLevel :: Ord Level where
  compare (Level l) (Level r) = compare l r


-- Define a type representing the size of something

newtype Size = Size
  { width :: Int
  , height :: Int
  }

instance decodeJsonSize :: DecodeJson Size where
  decodeJson json = do
    obj <- decodeJson json
    width <- obj .? "width"
    height <- obj .? "height"
    pure $ Size { width: width, height: height }


-- Define a type representing a single coordinate in a grid

newtype Row = Row Int
newtype Column = Column Int
data Coordinate = Coordinate Row Column


-- A type representing an item that has been placed on the layout

type PlacedItem =
  { id :: String
  , level :: Level
  , mode :: Maybe String
  , size :: Size
  }
