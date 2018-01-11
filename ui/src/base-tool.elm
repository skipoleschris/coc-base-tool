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
  }

initialModel : Model
initialModel = 
  { layout = emptyLayout
  , definition = Nothing
  , design = newDesign
  , importState = initialImportState
  }

newModelFromDefinition : TownHallDefinition -> Model
newModelFromDefinition definition =
  { layout = newLayout definition.level Nothing
  , definition = Just definition 
  , design = emptyDesign definition
  , importState = initialImportState
  }

newModelFromImport : TownHallDefinition -> Maybe String -> Design -> ImportState -> Model
newModelFromImport definition layoutName design importState =
  { layout = newLayout definition.level layoutName
  , definition = Just definition 
  , design = design
  , importState = importState
  }


-- UPDATE

type Msg = ChangeTownHallLevel String
         | TownHallDefinitionLoaded (Result Http.Error TownHallDefinition)
         | DesignUpdate DesignerMessage
         | ClearLayout
         | ExportLayout
         | ImportLayout ImportMessage
         | LayoutNameChange String

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


-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ viewLayout ChangeTownHallLevel LayoutNameChange model.layout
    , viewDesignEditor DesignUpdate model.design
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
-- Walls mode
