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

-- TYPES

type alias ImportState =
  { inProgress : Bool
  }


-- MODEL

initialImportState : ImportState
initialImportState =
  { inProgress = False }


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
  ( { state | inProgress = False}
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
      
  in
    div [ class ("import-dialog " ++ visibility) ]
        [ h3 [] [ text "Select File To Import" ]
        , div [ id "file-select"
              , class "file-select" 
              ] []
        , button [ onClick cancelMsg ] [ text "Cancel" ]
        ]


-- SUBSCRIPTION

initImportDataSubscription : (String -> msg) -> Sub msg
initImportDataSubscription msg =
  importData msg


