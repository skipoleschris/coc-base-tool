module Import exposing
  ( ImportState
  , ImportMessage
  , initialImportState
  , beginImportWorkflow
  , handleImportMessage
  , importOrInitialiseLayout
  , importDialog
  , initImportDataSubscription
  )

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)

import Common exposing (Level)
import ImportExportPort exposing (..)
import TownHallDefinitions exposing (TownHallDefinition)
import LayoutDefinitions exposing (LayoutDefinition, decodeFromJson)
import Designer exposing (Design, emptyDesign, removeHover, insertLayoutItems)

-- TYPES

type alias ImportState =
  { inProgress : Bool
  , layout : Maybe LayoutDefinition
  , error : String
  }

type ImportMessage = StartImportLayout
                   | CancelImportLayout
                   | CompleteImportLayout String


-- MODEL

initialImportState : ImportState
initialImportState =
  { inProgress = False 
  , layout = Nothing
  , error = "" 
  }

importInProgress : ImportState -> Bool
importInProgress state =
  state.inProgress && state.layout /= Nothing

importedLayoutName : ImportState -> Maybe String
importedLayoutName state =
  state.layout
    |> Maybe.map (\layout -> layout.layoutName)
    |> Maybe.andThen (\name -> if name == "" then Nothing else Just name)


-- UPDATE

beginImportWorkflow : ImportMessage
beginImportWorkflow = StartImportLayout

handleImportMessage : ImportMessage -> (Level -> Cmd msg) -> ImportState -> (ImportState, Cmd msg)
handleImportMessage msg nextStepCmd state =
  case msg of
    StartImportLayout ->
      showImportDialog state

    CancelImportLayout ->
      hideImportDialog state

    CompleteImportLayout data ->
      processImport data nextStepCmd state

showImportDialog : ImportState -> (ImportState, Cmd msg)
showImportDialog state =
  ( { state | inProgress = True }
  , initImport "file-select"
  )

hideImportDialog : ImportState -> (ImportState, Cmd msg)
hideImportDialog state =
  ( { state | inProgress = False }
  , cancelImport "file-select"
  )

processImport : String -> (Level -> Cmd msg) -> ImportState -> (ImportState, Cmd msg)
processImport data nextStepCmd state =
  let
    parseResult = decodeFromJson data
  in
    case parseResult of
      Ok layout ->
        ( { state | layout = Just layout
                  , error = "" }
        , nextStepCmd layout.townHallLevel
        )
      Err error ->
        ( { state | layout = Nothing
                  , error = error }
        , Cmd.none
        )

applyImport : ImportState -> Design -> ((ImportState, Design), Cmd msg)
applyImport state design =
  case state.layout of
    Just layout ->
      let
        (d, errors) = insertLayoutItems layout.items design
      in
        if List.isEmpty errors
        then           
          ( ( { state | inProgress = False }
            , removeHover d
            )
          , cancelImport "file-select"
          )
        else
          importError (List.foldr (++) "" errors) state design
    Nothing ->
      importError "Failed to complete import. Something expected went wrong." state design

importError : String -> ImportState -> Design -> ((ImportState, Design), Cmd msg)
importError msg state design =
  ( ( { state | inProgress = True
              , error = msg }
    , design
    )
  , Cmd.none
  )

importOrInitialiseLayout : TownHallDefinition ->
                           (TownHallDefinition -> a) ->
                           (TownHallDefinition -> Maybe String -> Design -> ImportState -> a) ->
                           ImportState ->
                           (a, Cmd msg)
importOrInitialiseLayout definition notImporting importing importState =
  if importInProgress importState
  then
    completeImportProcess definition importing importState
  else                           
    (notImporting definition, Cmd.none)

completeImportProcess : TownHallDefinition -> 
                        (TownHallDefinition -> Maybe String -> Design -> ImportState -> a) ->
                        ImportState -> 
                        (a, Cmd msg)
completeImportProcess definition complete importState =
  let
    design = emptyDesign definition
    
    ((pIS, pD), cmd) = applyImport importState design  
  in
    (complete definition (importedLayoutName importState) pD pIS, cmd)


-- VIEW

importDialog : ImportState -> (ImportMessage -> msg) -> Html msg
importDialog state cancelMsg =
  let
    visibility = 
      if state.inProgress
      then "visible"
      else "hidden"
     
    showError =
      if state.error == ""
      then "hidden"
      else "visible" 
  in
    div [ class ("import-dialog " ++ visibility) ]
        [ h3 [] [ text "Select File To Import" ]
        , div [ id "file-select"
              , class "file-select" 
              ] []
        , div [ class ("import-error " ++ showError) ] [ text state.error ]
        , button [ onClick (cancelMsg CancelImportLayout) ] [ text "Cancel" ]
        ]


-- SUBSCRIPTION

initImportDataSubscription : (ImportMessage -> msg) -> Sub msg
initImportDataSubscription msg =
  importData (CompleteImportLayout >> msg)
