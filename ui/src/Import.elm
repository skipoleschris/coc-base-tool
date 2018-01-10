module Import exposing
  ( ImportState
  , initialImportState
  , importInProgress
  , importedLayoutName
  , showImportDialog
  , hideImportDialog
  , processImport
  , applyImport
  , importDialog
  , initImportDataSubscription
  )

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)

import Common exposing (Level)
import ImportExportPort exposing (..)
import LayoutDefinitions exposing (LayoutDefinition, decodeFromJson)
import Pallette exposing (Pallette)
import Grid exposing (Grid)

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

importInProgress : ImportState -> Bool
importInProgress state =
  state.inProgress && state.layout /= Nothing

importedLayoutName : ImportState -> Maybe String
importedLayoutName state =
  state.layout
    |> Maybe.map (\layout -> layout.layoutName)
    |> Maybe.andThen (\name -> if name == "" then Nothing else Just name)


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

applyImport : ImportState -> Pallette -> Grid -> ((ImportState, Pallette, Grid), Cmd msg)
applyImport state pallette grid =
-- ERROR CASE
    --( ( { state | inProgress = True
    --            , error = "Inavlid base definition..." }
    --  , pallette
    --  , grid)
    --, Cmd.none
    --)

-- SUCCESS CASE
    ( ( { state | inProgress = False }
      , pallette
      , grid)
    , cancelImport "file-select"
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


