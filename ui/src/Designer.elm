module Designer exposing 
  ( Design
  , DesignMessages
  , newDesign
  , clearDesign
  , itemsInLayout
  , viewDesignEditor
  )

import Html exposing (..)

import Common exposing (Coordinate)
import Pallette exposing (..)
import Grid exposing (..)
import LayoutDefinitions exposing (LayoutItem)
import TownHallDefinitions exposing (TownHallDefinition)


-- TYPES

type alias Design =
  { pallette : Pallette
  , grid : Grid
  }

type alias DesignMessages msg =
  { itemSelectMsg : (String -> msg)
  , levelChangeMsg : (String -> String -> msg)
  , modeChangeMsg : (String -> String -> msg)
  , tileClickMsg : (Coordinate -> msg)
  , tileHoverMsg : (Coordinate -> msg)
  , removeHoverMsg : msg
  }

 -- MODEL

newDesign : Design
newDesign =  
  { pallette = emptyPallette
  , grid = makeGrid defaultSize
  }

emptyDesign : TownHallDefinition -> Design
emptyDesign definition =
  { pallette = freshPallette definition
  , grid = makeGrid defaultSize
  }

clearDesign : Maybe TownHallDefinition -> Design
clearDesign definition =
  definition
    |> Maybe.map emptyDesign
    |> Maybe.withDefault newDesign

itemsInLayout : Design -> List LayoutItem
itemsInLayout design =
  layoutItems design.grid


-- VIEW

viewDesignEditor : DesignMessages msg -> 
                   Design -> 
                   Html msg
viewDesignEditor m design =
  div [] [ viewPallette m.itemSelectMsg m.levelChangeMsg m.modeChangeMsg design.pallette
         , viewGrid m.tileClickMsg m.tileHoverMsg m.removeHoverMsg design.grid
         ]
