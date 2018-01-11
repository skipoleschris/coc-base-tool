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
  }

initialModel : Model
initialModel = 
  { layout = emptyLayout
  , definition = Nothing
  , design = newDesign
  , importState = initialImportState
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
         | ImportLayout ImportMessage
         | LayoutNameChange String

designMessages : DesignMessages Msg
designMessages =
  { itemSelectMsg = PalletteItemSelected
  , levelChangeMsg = PalletteLevelChange
  , modeChangeMsg = PalletteModeChange
  , tileClickMsg = TileClicked
  , tileHoverMsg = TileHover
  , removeHoverMsg = RemoveTileHover
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClearLayout ->
      ( { model | design = clearDesign model.definition }
      , Cmd.none
      )

    ExportLayout ->
      ( model
      , processExport model.layout (itemsInLayout model.design))

    ImportLayout importMsg ->
      handleImportMessage importMsg 
                          (loadTownHallDefinition TownHallDefinitionLoaded) 
                          model.importState
        |> Tuple.mapFirst (\state -> { model | importState = state })

    LayoutNameChange name ->
      ( { model | layout = changeLayoutName name model.layout }
      , Cmd.none
      )


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



-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ viewLayout ChangeTownHallLevel LayoutNameChange model.layout
    , viewDesignEditor designMessages model.design
    , viewToolbar ClearLayout ExportLayout ImportLayout
    , importDialog model.importState ImportLayout
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  initImportDataSubscription ImportLayout


-- INIT

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- TODO LIST
-- Code refactoring & clean up
