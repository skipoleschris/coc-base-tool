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
import Html.Attributes exposing (class, style)
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

type TileContent = Empty | Item PlacedItem | Reference Coordinate PlacedItem

type alias Grid = 
  { tiles : Dict Coordinate TileContent
  , size : Dimension
  }


-- HELPER FUNCTIONS

defaultSize : Dimension 
defaultSize = { width = 44, height = 44 }

makeGrid : Dimension -> Grid
makeGrid dimension =
  let
    tiles = List.map (\c -> (c, Empty)) (allCoordinates dimension) 
  in
    { tiles = Dict.fromList tiles
    , size = dimension
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


-- UPDATE

tileSelected : Coordinate -> Grid -> Maybe PlacedItem -> Grid
tileSelected coordinate grid item =
  let
    newTiles = 
      item 
        |> Maybe.map (createTilesForItem coordinate)
        |> Maybe.withDefault []

    updatedTiles = 
      newTiles
        |> List.foldl (\(coord, tile) tiles -> Dict.insert coord tile tiles) grid.tiles
  in
    { grid | tiles = updatedTiles }

createTilesForItem : Coordinate -> PlacedItem -> List (Coordinate, TileContent)
createTilesForItem coordinate item =
  let
    (top, left) = coordinate
    rows = List.range top (top + item.size.height - 1)
    cols = List.range left (left + item.size.width - 1)
  in
    rows 
      |> List.map (\row ->
          cols
            |> List.map (\col ->
                if top == row && left == col
                then ((row, col), Item item)
                else ((row, col), Reference coordinate item)
              )
        )
      |> List.concat

allPlacedItems : Grid -> List PlacedItem
allPlacedItems grid = 
  grid.tiles
    |> Dict.values
    |> List.filter (\t -> 
        case t of
          Item _ -> True
          _      -> False
      )
    |> List.map (\t -> 
        case t of 
          Item i -> i
          _      -> { id = "", level = 1, mode = Nothing, size = { width = 1, height = 1 } }
      )

-- VIEW

viewGrid : (Coordinate -> msg) -> Grid -> Html msg
viewGrid msg grid =
  div [ class "map" ] (List.map (makeRow msg grid) (rowIndexes grid))

makeRow : (Coordinate -> msg) -> Grid -> Row -> Html msg
makeRow msg grid row =
  div [ class "row" ] (List.map (makeCol msg grid row) (colIndexes grid))

makeCol : (Coordinate -> msg) -> Grid -> Row -> Column -> Html msg
makeCol msg grid row col =
  let
    tile = Dict.get (row, col) grid.tiles

    content =
      case tile of
        Just (Item i)      -> 
          [ ("background-image", "url('" ++ itemImage i ++ "')") ]
        Just (Reference c i) -> 
          [ ("background-image", "url('" ++ itemImage i ++ "')") 
          , ("background-position", itemImageOffset row col c)
          ]
        _                  -> 
          [ ("background-image", "url('data/images/grass.png')") ]
  in
    div [ class "tile", style content, onClick (msg (row, col)) ] []

itemImage : PlacedItem -> String
itemImage item =
  let 
    id = 
      if (String.startsWith "wall" item.id)
      then "wall"
      else item.id

    mode =
      item.mode 
        |> Maybe.map (\m -> "-" ++ m)
        |> Maybe.withDefault ""
  in
    "data/images/" ++ 
    id ++ 
    "/tile/" ++ 
    id ++ 
    "-" ++ 
    (toString item.level) ++ 
    mode ++
    ".png"      

itemImageOffset : Row -> Column -> Coordinate -> String
itemImageOffset row col coordinate =
  let
    (refRow, refCol) = 
      coordinate

    xpos = 
      -10 * (col - refCol)
      
    ypos =
      -10 * (row - refRow)
  in
    (toString xpos) ++ "px " ++ (toString ypos) ++ "px"
