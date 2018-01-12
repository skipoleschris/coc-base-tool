module Designer exposing 
  ( Design
  , DesignerMessage
  , newDesign
  , emptyDesign
  , clearDesign
  , itemsInLayout
  , removeHover
  , insertLayoutItems
  , handleDesignerMessage
  , setWallDrawing
  , viewDesignEditor
  )

import Html exposing (..)

import Common exposing (Coordinate)
import DesignerTypes exposing (..)
import Pallette exposing (..)
import Grid exposing (..)
import LayoutDefinitions exposing (LayoutItem)
import TownHallDefinitions exposing (TownHallDefinition)


-- TYPES

type alias WallDrawing =
  { enabled : Bool
  , active : Bool
  , erase : Bool
  , lastStart : Maybe Coordinate
  , lastEnd : Maybe Coordinate
  }

type alias Design =
  { pallette : Pallette
  , grid : Grid
  , wallDrawing : WallDrawing
  }

type DesignerMessage = PalletteItemSelected String
                     | PalletteLevelChange String String
                     | PalletteModeChange String String
                     | TileClicked Coordinate
                     | TileHover Coordinate
                     | RemoveTileHover
                     | WallDrawOn Coordinate
                     | WallDrawOff


 -- MODEL

newWallDrawing : WallDrawing
newWallDrawing =
  { enabled = True
  , active = False
  , erase = False 
  , lastStart = Nothing 
  , lastEnd = Nothing
  }

newDesign : Design
newDesign =  
  { pallette = emptyPallette
  , grid = makeGrid defaultSize
  , wallDrawing = newWallDrawing
  }

emptyDesign : TownHallDefinition -> Design
emptyDesign definition =
  { pallette = freshPallette definition
  , grid = makeGrid defaultSize
  , wallDrawing = newWallDrawing
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


-- BUILD DESIGN FROM LAYOUT ITEMS

insertLayoutItems : List LayoutItem -> Design -> (Design, List String)
insertLayoutItems items design =
  let
    placedItems = 
      List.map (toCoordinateAndPlacedItem design.pallette) items

    startState = (design, [])
  in
    List.foldl checkAndInsertItem startState placedItems

checkAndInsertItem : (Coordinate, PlacedItem) -> (Design, List String) -> (Design, List String)
checkAndInsertItem (coordinate, item) (design, errors) =
  let
    available = 
      isItemAvailable design.pallette item

    placable = 
      canPlaceItem coordinate design.grid item
  in
    case (available, placable) of
      (True, True) ->
        let 
          d = insertItem coordinate (Just item) design  
        in
          (d, errors)
      (True, False) ->
        ( design
        , (makeError coordinate item "it overlaps another item or is outside the grid") :: errors
        )
      _ ->
        ( design
        , (makeError coordinate item "is not available at this town hall level or all items of this type have been placed") :: errors
        )

insertItem : Coordinate -> Maybe PlacedItem -> Design -> Design
insertItem coordinate item design =
  let
    newGrid = tileSelected coordinate design.grid item
    placedItems = allPlacedItems newGrid
    newPallette = refreshPallette placedItems design.pallette
  in
    { design | pallette = newPallette
             , grid = newGrid 
    }

makeError : Coordinate -> PlacedItem -> String -> String
makeError coordinate item cause =
  let
    mode =
      item.mode 
        |> Maybe.map (\m -> " in " ++ m ++ " mode")
        |> Maybe.withDefault ""
      
  in
    "Item " ++ 
    item.id ++ 
    ", level " ++ 
    (toString item.level) ++ 
    mode ++
    ", cannot be positioned at tile (" ++ 
    (toString coordinate) ++ 
    "), because: " ++ 
    cause ++
    ". "

toCoordinateAndPlacedItem : Pallette -> LayoutItem -> (Coordinate, PlacedItem)
toCoordinateAndPlacedItem pallette item =
  ( (item.position.row, item.position.column)
  , { id = item.item
    , level = item.level
    , mode = item.mode
    , size = itemSize pallette item.item  
    }
  )


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

    WallDrawOn coordinate ->
      startWallDrawing coordinate design

    WallDrawOff ->
      cancelWallDrawing design

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
  cleaningUpWallDrawing doUpdateSelectedTile coordinate design

doUpdateSelectedTile : Coordinate -> Design -> Design
doUpdateSelectedTile coordinate design =
  let
      updated = insertItem coordinate (currentPalletteItem design.pallette) design
    in
      { updated | grid = tileHover coordinate updated.grid (currentPalletteItem updated.pallette) } 

hoverOverTile : Coordinate -> Design -> Design
hoverOverTile coordinate design =     
  let
    updatedDesign = 
      drawWallsIfRequired coordinate design

    newGrid = 
      tileHover coordinate updatedDesign.grid (currentPalletteItem updatedDesign.pallette)
  in
    { updatedDesign | grid = newGrid }

setWallDrawing : Bool -> Design -> Design
setWallDrawing state design =
  { design | wallDrawing = { enabled = state
                           , active = False
                           , erase = False
                           , lastStart = Nothing 
                           , lastEnd = Nothing }}

cancelWallDrawing : Design -> Design
cancelWallDrawing design =
  let
    wallDrawing = design.wallDrawing
  in
    { design | wallDrawing = { wallDrawing | active = False
                                           , lastEnd = wallDrawing.lastStart } }          

startWallDrawing : Coordinate -> Design -> Design
startWallDrawing coordinate design =
  let
    tileIsBlank = isBlankTile coordinate design.grid

    tileIsWall = isWallTile coordinate design.grid

    canBeActive = isWallSelected design.pallette && (tileIsBlank || tileIsWall)

    erase = tileIsWall

    existingWallDrawing = design.wallDrawing

    start =
      if canBeActive then Just coordinate else Nothing

    updated = { existingWallDrawing | active = canBeActive
                                    , erase = erase
                                    , lastStart = start }

    updatedDesign =
      if canBeActive
      then doUpdateSelectedTile coordinate design
      else design
  in
    { updatedDesign | wallDrawing = updated }          

cleaningUpWallDrawing : (Coordinate -> Design -> Design) -> Coordinate -> Design -> Design
cleaningUpWallDrawing f coordinate design =
  let
    updatedDesign =
      if Just coordinate == design.wallDrawing.lastEnd
      then design
      else f coordinate design      
  in
    cleanUpWallDrawing updatedDesign

cleanUpWallDrawing : Design -> Design
cleanUpWallDrawing design =
  let
    wallDrawing = design.wallDrawing

    updated = { wallDrawing | lastStart = Nothing
                            , lastEnd = Nothing }
  in
    { design | wallDrawing = updated }

drawWallsIfRequired : Coordinate -> Design -> Design
drawWallsIfRequired coordinate design =
  let
    wallDrawing = design.wallDrawing

    drawingWalls = wallDrawing.enabled && wallDrawing.active

    canDrawWall = (not wallDrawing.erase) && 
                  isBlankTile coordinate design.grid &&
                  isWallSelected design.pallette

    canEraseWall = wallDrawing.erase && isWallTile coordinate design.grid
  in
    if drawingWalls && (canDrawWall || canEraseWall)
    then doUpdateSelectedTile coordinate design  
    else design

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

    wallDrawOnMsg = WallDrawOn >> msg

    wallDrawOffMsg = msg WallDrawOff
  in
      
  div [] [ viewPallette itemSelectMsg levelChangeMsg modeChangeMsg design.pallette
         , viewGrid tileClickMsg tileHoverMsg removeHoverMsg wallDrawOnMsg wallDrawOffMsg design.grid
         ]
