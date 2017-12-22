module Grid exposing
  ( Coordinate
  , Grid
  , defaultSize
  , makeGrid
  , tileSelected
  , allPlacedItems
  , viewGrid
  )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Pallette exposing (PlacedItem)

-- TYPES

type alias Dimension = 
  { width : Int
  , height : Int
  }

type alias Row = Int
type alias Column = Int
type alias Coordinate = (Row, Column)

type alias Tile = 
  { 
  }

type alias Grid = 
  { tiles : Dict Coordinate Tile
  , size : Dimension
  , lastClicked : Maybe Coordinate
  , lastSelected : Maybe PlacedItem
  }


-- HELPER FUNCTIONS

defaultSize : Dimension 
defaultSize = { width = 44, height = 44 }

makeGrid : Dimension -> Grid
makeGrid dimension =
  let
    tiles = List.map (\c -> (c, tile)) (allCoordinates dimension) 
  in
    { tiles = Dict.fromList tiles
    , size = dimension
    , lastClicked = Nothing
    , lastSelected = Nothing
    }

allCoordinates : Dimension -> List Coordinate
allCoordinates size =
  let
    colRange = List.range 1 size.width
    rowRange = List.range 1 size.height
  in
    List.map (\r -> List.map (\c -> (r, c)) colRange) rowRange |> List.concat

rowIndexes : Grid -> List Row
rowIndexes grid =
  List.range 1 grid.size.height

colIndexes : Grid -> List Column
colIndexes grid =
  List.range 1 grid.size.width

tile : Tile
tile =
  { 
  }


-- UPDATE

tileSelected : Coordinate -> Grid -> Maybe PlacedItem -> Grid
tileSelected coordinate grid item =
  { grid | 
    lastClicked = Just coordinate 
  , lastSelected = item
  }

allPlacedItems : Grid -> List PlacedItem
allPlacedItems grid = []

-- VIEW

viewGrid : (Coordinate -> msg) -> Grid -> Html msg
viewGrid msg grid =
  div [ class "map" ] (List.map (makeRow msg grid) (rowIndexes grid))

makeRow : (Coordinate -> msg) -> Grid -> Row -> Html msg
makeRow msg grid row =
  div [ class "row" ] (List.map (makeCol msg grid row) (colIndexes grid))

makeCol : (Coordinate -> msg) -> Grid -> Row -> Column -> Html msg
makeCol msg grid row col =
  div [ class "tile grass", onClick (msg (row, col)) ] []

