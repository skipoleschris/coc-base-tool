module Designer exposing 
  ( Design
  , DesignerMessage
  , newDesign
  , emptyDesign
  , clearDesign
  , itemsInLayout
  , removeHover
  , handleDesignerMessage
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

type DesignerMessage = PalletteItemSelected String
                     | PalletteLevelChange String String
                     | PalletteModeChange String String
                     | TileClicked Coordinate
                     | TileHover Coordinate
                     | RemoveTileHover


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

removeHover : Design -> Design
removeHover design =
  { design | grid = noTileHover design.grid }

-- UPDATE

handleDesignerMessage : DesignerMessage -> Design -> Design
handleDesignerMessage msg design =
  case msg of
    PalletteItemSelected id ->
      { design | pallette = selectItem id design.pallette }

    PalletteLevelChange id level ->
      changePalletteLevel id level design

    PalletteModeChange id mode ->  
      { design | pallette = changeModeSelection id mode design.pallette }

    TileClicked coordinate ->
      updateSelectedTile coordinate design

    TileHover coordinate ->
      hoverOverTile coordinate design

    RemoveTileHover ->
      { design | grid = noTileHover design.grid }

changePalletteLevel : String -> String -> Design -> Design
changePalletteLevel id level design =
  String.toInt level
    |> Result.toMaybe 
    |> Maybe.map (\lvl ->
        { design | pallette = changeLevelSelection id lvl design.pallette }
      ) 
    |> Maybe.withDefault design

updateSelectedTile : Coordinate -> Design -> Design
updateSelectedTile coordinate design =
  let
    newGrid = tileSelected coordinate design.grid (currentPalletteItem design.pallette)
    placedItems = allPlacedItems newGrid
    newPallette = refreshPallette placedItems design.pallette
    finalGrid = tileHover coordinate newGrid (currentPalletteItem newPallette)
  in
    { pallette = newPallette, grid = finalGrid } 

hoverOverTile : Coordinate -> Design -> Design
hoverOverTile coordinate design =     
  let
    newGrid = tileHover coordinate design.grid (currentPalletteItem design.pallette)
  in
    { design | grid = newGrid }


-- VIEW

viewDesignEditor : (DesignerMessage -> msg) -> Design -> Html msg
viewDesignEditor msg design =
  let
    itemSelectMsg = PalletteItemSelected >> msg

    levelChangeMsg =
      curry <| uncurry PalletteLevelChange >> msg

    modeChangeMsg = 
      curry <| uncurry PalletteModeChange >> msg

    tileClickMsg = TileClicked >> msg

    tileHoverMsg = TileHover >> msg

    removeHoverMsg = msg RemoveTileHover
  in
      
  div [] [ viewPallette itemSelectMsg levelChangeMsg modeChangeMsg design.pallette
         , viewGrid tileClickMsg tileHoverMsg removeHoverMsg design.grid
         ]
