module DesignerTypes exposing
  ( PlacedItem
  )

import Common exposing (Level, Size)

type alias PlacedItem =
  { id : String
  , level : Level
  , mode : Maybe String
  , size : Size
  }
