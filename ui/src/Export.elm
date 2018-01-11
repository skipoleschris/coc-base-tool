module Export exposing 
  ( processExport
  )

import Common exposing (Level)
import Layouts exposing (Layout)
import LayoutDefinitions exposing (LayoutItem, encodeToJson)
import ImportExportPort exposing (..)


-- UPDATE

processExport : Layout -> List LayoutItem -> Cmd msg
processExport layout items =
  let
    name =
      layout.layoutName
        |> Maybe.withDefault ""

    filename =
      layout.layoutName
        |> Maybe.map (\n -> n ++ ".json")
        |> Maybe.withDefault "layout.json"

    data =
      layout.townHallLevel
        |> Maybe.map (\thLevel -> encodeToJson name thLevel items)
        |> Maybe.map (\json -> filename ++ "|" ++ json)
  in
    data
      |> Maybe.map export
      |> Maybe.withDefault Cmd.none
