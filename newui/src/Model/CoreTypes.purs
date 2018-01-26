module Model.CoreTypes where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))

-- Define the Level type for different building and troop levels

newtype Level = Level Int

instance showLevel :: Show Level where
  show (Level lvl) = show lvl

instance eqLevel :: Eq Level where
  eq (Level left) (Level right) = left == right


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

