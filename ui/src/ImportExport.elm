port module ImportExport exposing
  ( buildExportJson
  , export
  )

import Json.Encode exposing (..)

import TownHallDefinitions exposing (Level)
import Grid exposing (LayoutItem)

buildExportJson : Level -> List LayoutItem -> String
buildExportJson thLevel items =
  let
    exportItems =
      items
        |> List.map itemToObject
        |> list

    exportJson =
      object [ ("townHallLevel", int thLevel)
             , ("items", exportItems)
             ]
  in
    encode 0 exportJson      

itemToObject : LayoutItem -> Value
itemToObject item =
  let
    (row, column) = item.position

    positionObj =
      object [ ("row", int row)
             , ("column", int column)]

    mode =
      item.mode
        |> Maybe.map (\mode -> [ ("mode", string mode) ])
        |> Maybe.withDefault []
  in
    object ([ ("position", positionObj)
            , ("building", string item.item)
            , ("level", int item.level)
            ] ++ mode)      


port export : String -> Cmd msg
