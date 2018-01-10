module Import exposing
  ( ImportState
  , initialImportState
  , showImportDialog
  , hideImportDialog
  , processImport
  , importDialog
  , initImportDataSubscription
  )

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)

import ImportExportPort exposing (..)
import LayoutDefinitions exposing (LayoutDefinition, decodeFromJson)


-- TYPES

type alias ImportState =
  { inProgress : Bool
  , layout : Maybe LayoutDefinition
  , error : String
  }


-- MODEL

initialImportState : ImportState
initialImportState =
  { inProgress = False 
  , layout = Nothing
  , error = "" 
  }


-- UPDATE

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

processImport : String -> ImportState -> (ImportState, Cmd msg)
processImport data state =
  let
    parseResult = decodeFromJson data
  in
    case parseResult of
      Ok layout ->
        ( { state | layout = Just layout
                  , error = "" }
        , Cmd.none
        )
      Err error ->
        ( { state | layout = Nothing
                  , error = error }
        , Cmd.none
        )


-- VIEW

importDialog : ImportState -> msg -> Html msg
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
        , button [ onClick cancelMsg ] [ text "Cancel" ]
        ]


-- SUBSCRIPTION

initImportDataSubscription : (String -> msg) -> Sub msg
initImportDataSubscription msg =
  importData msg


