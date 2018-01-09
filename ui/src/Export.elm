module Export exposing 
  ( processExport
  )

import Common exposing (Level)
import LayoutDefinitions exposing (LayoutItem, encodeToJson)
import ImportExportPort exposing (..)


-- UPDATE

processExport : Maybe String -> Maybe Level -> List LayoutItem -> Cmd msg
processExport layoutName townHallLevel items =
  let
    name =
      layoutName
        |> Maybe.withDefault ""

    filename =
      layoutName
        |> Maybe.map (\n -> n ++ ".json")
        |> Maybe.withDefault "layout.json"

    data =
      townHallLevel
        |> Maybe.map (\thLevel -> encodeToJson name thLevel items)
        |> Maybe.map (\json -> filename ++ "|" ++ json)
  in
    data
      |> Maybe.map export
      |> Maybe.withDefault Cmd.none
