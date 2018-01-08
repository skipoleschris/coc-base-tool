module LayoutDefinitions exposing
  ( Position
  , LayoutItem
  , LayoutDefinition
  , encodeToJson
  , decodeFromJson
  )

import Maybe exposing (..)
import Json.Encode as E exposing (object, int, string, encode, Value)
import Json.Decode as D exposing (int, string, nullable, list, Decoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

import Common exposing (Level)

-- TYPES

type alias Position =
  { row : Int
  , column : Int
  }

type alias LayoutItem =
  { position : Position
  , item : String
  , level : Level
  , mode : Maybe String
  }

type alias LayoutDefinition =
  { layoutName : String
  , townHallLevel : Level
  , items : List LayoutItem
  }


-- ENCODING

encodeToJson : String -> Level -> List LayoutItem -> String
encodeToJson layoutName thLevel items =
  let
    exportItems =
      items
        |> List.map itemToObject
        |> E.list

    exportJson =
      E.object [ ("layoutName", E.string layoutName)
             , ("townHallLevel", E.int thLevel)
             , ("items", exportItems)
             ]
  in
    E.encode 0 exportJson      

itemToObject : LayoutItem -> E.Value
itemToObject item =
  let
    positionObj =
      E.object [ ("row", E.int item.position.row)
             , ("column", E.int item.position.column)]

    mode =
      item.mode
        |> Maybe.map (\mode -> [ ("mode", E.string mode) ])
        |> Maybe.withDefault []
  in
    E.object ([ ("position", positionObj)
            , ("building", E.string item.item)
            , ("level", E.int item.level)
            ] ++ mode)      


-- DECODING

decodeFromJson : String -> Result String LayoutDefinition
decodeFromJson json =
  D.decodeString layoutDefinitionDecoder json

positionDecoder : D.Decoder Position
positionDecoder =
  decode Position
    |> required "row" D.int
    |> required "column" D.int

layoutItemDecoder : D.Decoder LayoutItem
layoutItemDecoder =
  decode LayoutItem
    |> required "position" positionDecoder
    |> required "item" D.string
    |> required "level" D.int
    |> optional "mode" (nullable D.string) Nothing

layoutDefinitionDecoder : D.Decoder LayoutDefinition
layoutDefinitionDecoder =
  decode LayoutDefinition
    |> required "layoutName" D.string
    |> required "townHallLevel" D.int
    |> required "items" (D.list layoutItemDecoder)


