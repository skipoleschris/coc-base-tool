module Grid exposing
  ( Coordinate
  , Grid
  , LayoutItem
  , defaultSize
  , makeGrid
  , layoutItems
  , tileSelected
  , tileHover
  , noTileHover
  , allPlacedItems
  , viewGrid
  )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseOver, onMouseOut)

import TownHallDefinitions exposing (Level)
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
  , hoverState : HoverState
  , size : Dimension
  }

type alias HoverState = 
  { disabledHighight : List Coordinate
  , deleteHighlight : List Coordinate
  , placeHighlight : List Coordinate
  }

type alias LayoutItem =
  { position : Coordinate
  , item : String
  , level : Level
  , mode : Maybe String
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
    , hoverState = emptyHoverState
    }

emptyHoverState : HoverState
emptyHoverState = 
  { disabledHighight = []
  , deleteHighlight = []
  , placeHighlight = []
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

layoutItems : Grid -> List LayoutItem
layoutItems grid =
  let
    items =
      grid.tiles
        |> Dict.toList
        |> List.foldl (\item result -> 
            case (Tuple.second item) of
              Item i -> 
                { position = (Tuple.first item)
                , item = i.id
                , level = i.level
                , mode = i.mode
                } :: result
              _      -> 
                result
          ) []
  in
    items      

-- UPDATE

tileHover : Coordinate -> Grid -> Maybe PlacedItem -> Grid
tileHover coordinate grid item =
  let
    newHoverState = 
      grid.tiles
        |> Dict.get coordinate
        |> Maybe.map (\tile ->
            case tile of
              Empty         ->
                case item of
                  Nothing ->
                    { disabledHighight = [ coordinate ]
                    , deleteHighlight = [] 
                    , placeHighlight = []
                    }
                  Just i ->
                    if canPlaceItem coordinate grid i
                    then
                      { disabledHighight = []
                      , deleteHighlight = [] 
                      , placeHighlight = coordinatesFor coordinate i.size
                      }
                    else
                      { disabledHighight = coordinatesFor coordinate i.size 
                      , deleteHighlight = [] 
                      , placeHighlight = []
                      }
              Item i        ->
                { disabledHighight = []
                , deleteHighlight = coordinatesFor coordinate i.size
                , placeHighlight = []
                }
              Reference c i ->
                { disabledHighight = []
                , deleteHighlight = coordinatesFor c i.size
                , placeHighlight = []
                }
          )
        |> Maybe.withDefault grid.hoverState
  in 
    { grid | hoverState = newHoverState }

coordinatesFor : Coordinate -> Dimension -> List Coordinate
coordinatesFor coordinate size =
  let
    (top, left) = coordinate
    rows = List.range top (top + size.height - 1)
    cols = List.range left (left + size.width - 1)
  in
    rows 
      |> List.map (\row ->
          cols
            |> List.map (\col -> (row, col))
        )
      |> List.concat
      
noTileHover : Grid -> Grid
noTileHover grid =
  { grid | hoverState = emptyHoverState }

tileSelected : Coordinate -> Grid -> Maybe PlacedItem -> Grid
tileSelected coordinate grid item =
  let
    actionedGrid =
      if isPopulated coordinate grid
      then removeItem coordinate grid
      else placeItem coordinate grid item
  in
    tileHover coordinate actionedGrid item      

isPopulated : Coordinate -> Grid -> Bool
isPopulated coordinate grid =
  grid.tiles
    |> Dict.get coordinate
    |> Maybe.map (\t -> t /= Empty)
    |> Maybe.withDefault False

removeItem : Coordinate -> Grid -> Grid
removeItem coordinate grid =
  let
    removeCriteria =
      grid.tiles
        |> Dict.get coordinate
        |> Maybe.map (\t ->
            case t of
              Item i -> (coordinate, i.size)
              Reference c i -> (c, i.size)
              Empty -> (coordinate, { width = 1, height = 1 })
          )

    newTiles =
      removeCriteria
        |> Maybe.map (emptyTilesForCriteria)
        |> Maybe.withDefault []

    updatedTiles = 
      newTiles
        |> List.foldl (\(coord, tile) tiles -> Dict.insert coord tile tiles) grid.tiles
  in
    { grid | tiles = updatedTiles }        

emptyTilesForCriteria : (Coordinate, Dimension) -> List (Coordinate, TileContent)
emptyTilesForCriteria (coordinate, size) =
  let
    (top, left) = coordinate
    rows = List.range top (top + size.height - 1)
    cols = List.range left (left + size.width - 1)
  in
    rows 
      |> List.map (\row ->
          cols
            |> List.map (\col -> ((row, col), Empty))
        )
      |> List.concat


placeItem : Coordinate -> Grid -> Maybe PlacedItem -> Grid
placeItem coordinate grid item =
  let
    canPlace =
      item 
        |> Maybe.map (canPlaceItem coordinate grid)
        |> Maybe.withDefault False

    newTiles = 
      item 
        |> Maybe.map (createTilesForItem coordinate)
        |> Maybe.withDefault []

    updatedTiles = 
      newTiles
        |> List.foldl (\(coord, tile) tiles -> Dict.insert coord tile tiles) grid.tiles
  in
    if canPlace
    then { grid | tiles = updatedTiles }
    else grid

canPlaceItem : Coordinate -> Grid -> PlacedItem -> Bool
canPlaceItem coordinate grid item =
  let
    (top, left) = coordinate
    rows = List.range top (top + item.size.height - 1)
    cols = List.range left (left + item.size.width - 1)
  in
    rows 
      |> List.map (\row ->
          cols
            |> List.map (\col ->
              Dict.get (row, col) grid.tiles
                |> Maybe.map (\t -> t == Empty)
                |> Maybe.withDefault False
              )
        )
      |> List.concat
      |> List.all identity

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

viewGrid : (Coordinate -> msg) -> (Coordinate -> msg) -> msg -> Grid -> Html msg
viewGrid clickMsg hoverMsg noHoverMsg grid =
  div [ class "map" 
      , onMouseOut noHoverMsg
      ] (List.map (makeRow clickMsg hoverMsg grid) (rowIndexes grid))

makeRow : (Coordinate -> msg) -> (Coordinate -> msg) -> Grid -> Row -> Html msg
makeRow clickMsg hoverMsg grid row =
  div [ class "row" ] (List.map (makeCol clickMsg hoverMsg grid row) (colIndexes grid))

makeCol : (Coordinate -> msg) -> (Coordinate -> msg) -> Grid -> Row -> Column -> Html msg
makeCol clickMsg hoverMsg grid row col =
  let
    tile = Dict.get (row, col) grid.tiles

    content =
      case tile of
        Just (Item i)        -> 
          [ ("background-image", "url('" ++ itemImage i ++ "')") ]
        Just (Reference c i) -> 
          [ ("background-image", "url('" ++ itemImage i ++ "')") 
          , ("background-position", itemImageOffset row col c)
          ]
        _                    -> 
          case (isEven row, isEven col) of
            (False, False) -> [ ("background-image", "url('data/images/grass.png')") ]
            (False, True)  -> [ ("background-image", "url('data/images/mud.png')") ]
            (True,  False) -> [ ("background-image", "url('data/images/mud.png')") ]
            (True,  True)  -> [ ("background-image", "url('data/images/grass.png')") ]

    hovered =
      if List.member (row, col) grid.hoverState.disabledHighight
      then [ ("box-shadow", "inset 0 0 0 15px rgba(255, 0, 0, 0.4") ]
      else
        if List.member (row, col) grid.hoverState.deleteHighlight
        then [ ("box-shadow", "inset 0 0 0 15px rgba(255, 127, 0, 0.6") ]
        else
          if List.member (row, col) grid.hoverState.placeHighlight
          then [ ("box-shadow", "inset 0 0 0 15px rgba(0, 0, 255, 0.4") ]
          else []
  in
    div [ class "tile"
        , style (hovered ++ content)
        , onClick (clickMsg (row, col)) 
        , onMouseOver (hoverMsg (row, col))
        ] []


isEven : Int -> Bool
isEven i = i % 2 == 0

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
      -15 * (col - refCol)
      
    ypos =
      -15 * (row - refRow)
  in
    (toString xpos) ++ "px " ++ (toString ypos) ++ "px"
