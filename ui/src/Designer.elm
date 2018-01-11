module Designer exposing 
  ( Design
  , newDesign
  , viewDesignEditor
  )

import Html exposing (..)

import Common exposing (Coordinate)
import Pallette exposing (..)
import Grid exposing (..)


-- TYPES

type alias Design =
  { pallette : Pallette
  , grid : Grid
  }


 -- MODEL

newDesign : Design
newDesign =  
  { pallette = emptyPallette
  , grid = makeGrid defaultSize
  }


-- VIEW

viewDesignEditor : (String -> msg) ->
                   (String -> String -> msg) ->
                   (String -> String -> msg) ->
                   (Coordinate -> msg) ->
                   (Coordinate -> msg) ->
                   msg -> 
                   Design -> 
                   Html msg
viewDesignEditor itemSelectMsg levelChangeMsg modeChangeMsg tileClickMsg tileHoverMsg removeHoverMsg design =
  div [] [ viewPallette itemSelectMsg levelChangeMsg modeChangeMsg design.pallette
         , viewGrid tileClickMsg tileHoverMsg removeHoverMsg design.grid
         ]
