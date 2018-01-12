import Html exposing (..)
import Http exposing (Error)

import Common exposing (..)
import TownHallDefinitions exposing (TownHallDefinition, loadTownHallDefinition)
import Layouts exposing (..)
import Designer exposing (..)
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
  , toolbarState : ToolbarState
  , testMsg : String
  }

initialModel : Model
initialModel = 
  { layout = emptyLayout
  , definition = Nothing
  , design = newDesign
  , importState = initialImportState
  , toolbarState = initialToolbar True
  , testMsg = "Not over"
  }

newModelFromDefinition : TownHallDefinition -> Model
newModelFromDefinition definition =
  { layout = newLayout definition.level Nothing
  , definition = Just definition 
  , design = emptyDesign definition
  , importState = initialImportState
  , toolbarState = initialToolbar True
  , testMsg = "Not over"
  }

newModelFromImport : TownHallDefinition -> Maybe String -> Design -> ImportState -> Model
newModelFromImport definition layoutName design importState =
  { layout = newLayout definition.level layoutName
  , definition = Just definition 
  , design = design
  , importState = importState
  , toolbarState = initialToolbar design.wallDrawing.enabled
  , testMsg = "Not over"
  }


-- UPDATE

type Msg = ChangeTownHallLevel String
         | TownHallDefinitionLoaded (Result Http.Error TownHallDefinition)
         | DesignUpdate DesignerMessage
         | ClearLayout
         | ExportLayout
         | ImportLayout ImportMessage
         | LayoutNameChange String
         | ToolbarChange ToolbarMessage
         | MouseOverTest

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTownHallLevel level ->
      ( model
      , changeTownHallLevel level (loadTownHallDefinition TownHallDefinitionLoaded)
      )

    TownHallDefinitionLoaded (Ok definition) ->
      importOrInitialiseLayout definition 
                               newModelFromDefinition
                               newModelFromImport
                               model.importState

    TownHallDefinitionLoaded (Err err) ->
      ( initialModel
      , Cmd.none
      )

    DesignUpdate designMsg ->
      ( { model | design = handleDesignerMessage designMsg model.design }
      , Cmd.none
      )

    ClearLayout ->
      ( { model | design = clearDesign model.definition 
                , toolbarState = initialToolbar True }
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

    ToolbarChange toolbarMsg ->
      let
        toolbarState = updateToolbar toolbarMsg model.toolbarState
      in
        ( { model | toolbarState = toolbarState
                  , design = setWallDrawing toolbarState.wallDrawingMode model.design }
        , Cmd.none
        )

    MouseOverTest ->
      ( { model | testMsg = "Mouse went over" }
      , Cmd.none
      )

-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ viewLayout ChangeTownHallLevel LayoutNameChange model.layout
    , viewDesignEditor DesignUpdate model.design
    , viewToolbar ClearLayout ExportLayout ImportLayout ToolbarChange model.toolbarState
    , importDialog model.importState ImportLayout
    , foo
    ]

foo : Html Msg
foo model =
  div [ onMouseOver MouseOverTest ] [ text model.testMsg ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  initImportDataSubscription ImportLayout


-- INIT

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- TODO LIST
-- Code refactoring & clean up
-- Walls mode
