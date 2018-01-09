import Html exposing (..)
import Html.Attributes exposing (rel, href, class, size, placeholder, id)
import Html.Events exposing (onInput, onClick)
import Http exposing (Error)

import Common exposing (..)
import TownHallDefinitions exposing (TownHallDefinition, loadTownHallDefinition, townHallLevelSelect)
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
  { grid : Grid
  , townHallLevels : List Level
  , townHallLevel : Maybe Level
  , layoutName : Maybe String
  , definition : Maybe TownHallDefinition
  , pallette : Pallette
  , importState : ImportState
  , debug : Maybe Error
  }

model : Model
model = 
  { grid = makeGrid defaultSize
  , townHallLevels = [11, 10, 9, 8, 7, 6, 5, 4, 3]
  , townHallLevel = Nothing
  , layoutName = Nothing
  , definition = Nothing
  , pallette = emptyPallette
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
      ( { model | 
          townHallLevel = String.toInt level |> Result.toMaybe 
        }
      , String.toInt level 
          |> Result.toMaybe 
          |> Maybe.map (loadTownHallDefinition TownHallDefinitionLoaded)
          |> Maybe.withDefault Cmd.none
      )

    TownHallDefinitionLoaded (Ok definition) ->
      ( { model | 
          definition = Just definition 
        , pallette = freshPallette definition
        , grid = makeGrid defaultSize
        }
      , Cmd.none
      )

    TownHallDefinitionLoaded (Err err) ->
      ( { model | 
          townHallLevel = Nothing
        , definition = Nothing
        , pallette = emptyPallette
        , debug = Just err 
        }
      , Cmd.none
      )

    PalletteItemSelected id ->
      ( { model | pallette = selectItem id model.pallette }
      , Cmd.none
      )

    PalletteLevelChange id level ->
      (
        String.toInt level |> Result.toMaybe |> Maybe.map (\lvl ->
          { model | pallette = changeLevelSelection id lvl model.pallette }
        ) |> Maybe.withDefault model
      , Cmd.none
      )

    PalletteModeChange id mode ->  
      ( { model | pallette = changeModeSelection id mode model.pallette }
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
      ( { model | grid = noTileHover model.grid }
      , Cmd.none
      )

    ClearLayout ->
      ( clearLayout model
      , Cmd.none
      )

    ExportLayout ->
      ( model
      , processExport model.layoutName model.townHallLevel (layoutItems model.grid))

    StartImportLayout ->
      processImportStep model showImportDialog

    CancelImportLayout ->
      processImportStep model hideImportDialog

    ImportLayout data ->
      processImportStep model (processImport data)

    LayoutNameChange name ->
      ( { model | layoutName = if name == "" then Nothing else Just name }
      , Cmd.none
      )

updateSelectedTile : Coordinate -> Model -> Model
updateSelectedTile coordinate model =
  let
    newGrid = tileSelected coordinate model.grid (currentPalletteItem model.pallette)
    placedItems = allPlacedItems newGrid
    newPallette = refreshPallette placedItems model.pallette
    finalGrid = tileHover coordinate newGrid (currentPalletteItem newPallette)
  in
    { model | grid = finalGrid, pallette = newPallette }

hoverOverTile : Coordinate -> Model -> Model
hoverOverTile coordinate model =     
  let
    newGrid = tileHover coordinate model.grid (currentPalletteItem model.pallette)
  in
    { model | grid = newGrid }

clearLayout : Model -> Model
clearLayout model =
  case model.definition of
    Nothing ->
      { model | grid = makeGrid defaultSize }
    Just definition ->
      { model |
        pallette = freshPallette definition 
      , grid = makeGrid defaultSize 
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
    [ townHallLevelSelect ChangeTownHallLevel model.townHallLevels
    , layoutTitle 
    , viewGrid TileClicked TileHover RemoveTileHover model.grid
    , viewPallette PalletteItemSelected PalletteLevelChange PalletteModeChange model.pallette
    , viewToolbar ClearLayout ExportLayout StartImportLayout
    , importDialog model.importState CancelImportLayout
    ]

layoutTitle : Html Msg
layoutTitle =
  div [ class "layout-title" ]
      [ label [] [ text "Layout Title: " ]
      , input [ size 50
              , placeholder "Enter name..."
              , onInput LayoutNameChange ] 
              []
      ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  initImportDataSubscription ImportLayout


-- INIT

init : (Model, Cmd Msg)
init = (model, Cmd.none)



-- TODO LIST
-- Export layout
-- Import layout
-- Code refactoring & clean up
