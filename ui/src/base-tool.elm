import Html exposing (..)
import Html.Attributes exposing (rel, href, class, size, placeholder, id, value)
import Html.Events exposing (onInput, onClick)
import Http exposing (Error)

import Common exposing (..)
import TownHallDefinitions exposing (TownHallDefinition, loadTownHallDefinition)
import Layouts exposing (..)
import Designer exposing (..)
import Pallette exposing (..)
import Grid exposing (..)
import Toolbar exposing (..)
import Import exposing (..)
import Export exposing (..)

main = 
  Html.program 
    { init = init
    , update = update
    , view = view 
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { layout : Layout
  , definition : Maybe TownHallDefinition
  , design : Design
  , importState : ImportState
  , debug : Maybe Error
  }

initialModel : Model
initialModel = 
  { layout = emptyLayout
  , definition = Nothing
  , design = newDesign
  , importState = initialImportState
  , debug = Nothing
  }


-- UPDATE

type Msg = ChangeTownHallLevel String
         | TownHallDefinitionLoaded (Result Http.Error TownHallDefinition)
         | PalletteItemSelected String
         | PalletteLevelChange String String
         | PalletteModeChange String String
         | TileClicked Coordinate
         | TileHover Coordinate
         | RemoveTileHover
         | ClearLayout
         | ExportLayout
         | StartImportLayout
         | CancelImportLayout
         | ImportLayout String
         | LayoutNameChange String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTownHallLevel level ->
      ( model
      , String.toInt level 
          |> Result.toMaybe 
          |> Maybe.map (loadTownHallDefinition TownHallDefinitionLoaded)
          |> Maybe.withDefault Cmd.none
      )

    TownHallDefinitionLoaded (Ok definition) ->
      if importInProgress model.importState
      then
        let
          pallette = freshPallette definition

          grid = makeGrid defaultSize
          
          ((pIS, pP, pG), cmd) = applyImport model.importState pallette grid  
        in
          ( { model | 
              layout = newLayout definition.level (importedLayoutName model.importState) model.layout
            , definition = Just definition
            , design = { pallette =pP, grid = noTileHover pG } 
            , importState = pIS
            }
          , cmd
          )
      else 
        ( { model | 
            layout = newLayout definition.level Nothing model.layout
          , definition = Just definition 
          , design = { pallette = freshPallette definition, grid = makeGrid defaultSize }
          }
        , Cmd.none
        )

    TownHallDefinitionLoaded (Err err) ->
      ( { model | 
          layout = emptyLayout
        , definition = Nothing
        , design = newDesign
        , debug = Just err 
        }
      , Cmd.none
      )

    PalletteItemSelected id ->
      ( { model | design = { pallette = selectItem id model.design.pallette, grid = model.design.grid } }
      , Cmd.none
      )

    PalletteLevelChange id level ->
      (
        String.toInt level |> Result.toMaybe |> Maybe.map (\lvl ->
          { model | design = { pallette = changeLevelSelection id lvl model.design.pallette, grid = model.design.grid } }
        ) |> Maybe.withDefault model
      , Cmd.none
      )

    PalletteModeChange id mode ->  
      ( { model | design = { pallette = changeModeSelection id mode model.design.pallette, grid = model.design.grid } }
      , Cmd.none
      )

    TileClicked coordinate ->
      ( updateSelectedTile coordinate model
      , Cmd.none
      )

    TileHover coordinate ->
      ( hoverOverTile coordinate model
      , Cmd.none
      )

    RemoveTileHover ->
      ( { model | design = { grid = noTileHover model.design.grid, pallette = model.design.pallette } }
      , Cmd.none
      )

    ClearLayout ->
      ( clearLayout model
      , Cmd.none
      )

    ExportLayout ->
      ( model
      , processExport model.layout.layoutName model.layout.townHallLevel (layoutItems model.design.grid))

    StartImportLayout ->
      processImportStep model showImportDialog

    CancelImportLayout ->
      processImportStep model hideImportDialog

    ImportLayout data ->
      processImportStep model (processImport data (loadTownHallDefinition TownHallDefinitionLoaded))

    LayoutNameChange name ->
      ( { model | layout = changeLayoutName name model.layout }
      , Cmd.none
      )

updateSelectedTile : Coordinate -> Model -> Model
updateSelectedTile coordinate model =
  let
    newGrid = tileSelected coordinate model.design.grid (currentPalletteItem model.design.pallette)
    placedItems = allPlacedItems newGrid
    newPallette = refreshPallette placedItems model.design.pallette
    finalGrid = tileHover coordinate newGrid (currentPalletteItem newPallette)
  in
    { model | design = { grid = finalGrid, pallette = newPallette } }

hoverOverTile : Coordinate -> Model -> Model
hoverOverTile coordinate model =     
  let
    newGrid = tileHover coordinate model.design.grid (currentPalletteItem model.design.pallette)
  in
    { model | design = { grid = newGrid, pallette = model.design.pallette } }

clearLayout : Model -> Model
clearLayout model =
  case model.definition of
    Nothing ->
      { model | design = { grid = makeGrid defaultSize, pallette = emptyPallette } }
    Just definition ->
      { model | design = {
        pallette = freshPallette definition 
      , grid = makeGrid defaultSize }
      }

processImportStep : Model -> (ImportState -> (ImportState, Cmd msg)) -> (Model, Cmd msg)
processImportStep model f =
  let
    (state, cmd) = f model.importState
  in
    ( { model | importState = state }
    , cmd
    )


-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ viewLayout ChangeTownHallLevel LayoutNameChange model.layout
    , viewDesignEditor PalletteItemSelected PalletteLevelChange PalletteModeChange TileClicked TileHover RemoveTileHover model.design
    , viewToolbar ClearLayout ExportLayout StartImportLayout
    , importDialog model.importState CancelImportLayout
    ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  initImportDataSubscription ImportLayout


-- INIT

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)



-- TODO LIST
-- Import layout
-- Code refactoring & clean up
