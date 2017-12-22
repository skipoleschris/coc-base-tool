module TownHallDefinitions exposing 
  ( Level
  , TownHallDefinition
  , AllowedBuilding
  , Size
  , Mode
  , Walls
  , loadTownHallDefinition
  , townHallLevelSelect
  )

import Maybe exposing (..)
import Http exposing (get, send, Error)
import Json.Decode exposing (int, string, nullable, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

import Html exposing (..)
import Html.Attributes exposing (value, disabled, selected)
import Html.Events exposing (onInput)


-- TYPES

type alias TownHallDefinition =
  { level : Level
  , defences : List AllowedBuilding
  , army : List AllowedBuilding
  , resources : List AllowedBuilding
  , traps : List AllowedBuilding
  , troops : List Troop
  , spells : List Spell
  , walls : List Walls
  }

type alias Level = Int

type alias AllowedBuilding =
  { id : String
  , name : String
  , quantity : Int
  , maxLevel : Level
  , minLevel : Maybe Level
  , size : Size 
  , modes : List Mode
  }

type alias Size =
  { width : Int
  , height : Int
  }

type alias Mode =
  { id : String
  , name : String
  , maxAllowed : Maybe Int 
  , minLevel : Maybe Int
  }

type alias Troop  =
  { id : String
  , name : String
  , maxLevel : Level 
  }

type alias Spell  =
  { id : String
  , name : String
  , maxLevel : Level 
  }

type alias Walls =
  { quantity : Int
  , maxLevel : Level
  }


-- DECODERS

townHallDefinitionDecoder : Decoder TownHallDefinition
townHallDefinitionDecoder =
  decode TownHallDefinition
    |> required "level" int
    |> required "defences" (list allowedBuildingDecoder)
    |> required "army" (list allowedBuildingDecoder)
    |> required "resources" (list allowedBuildingDecoder)
    |> required "traps" (list allowedBuildingDecoder)
    |> required "troops" (list troopDecoder)
    |> required "spells" (list spellDecoder)
    |> required "walls" (list wallsDecoder)

allowedBuildingDecoder : Decoder AllowedBuilding
allowedBuildingDecoder =
  decode AllowedBuilding
    |> required "id" string
    |> required "name" string
    |> required "qty" int
    |> required "level" int
    |> optional "minLevel" (nullable int) Nothing
    |> required "size" sizeDecoder
    |> optional "modes" (list modeDecoder) []

sizeDecoder : Decoder Size
sizeDecoder =
  decode Size
    |> required "width" int
    |> required "height" int

modeDecoder : Decoder Mode 
modeDecoder =
  decode Mode
    |> required "id" string
    |> required "name" string
    |> optional "max" (nullable int) Nothing
    |> optional "level" (nullable int) Nothing

troopDecoder : Decoder Troop
troopDecoder =
  decode Troop
    |> required "id" string
    |> required "name" string
    |> required "level" int

spellDecoder : Decoder Spell
spellDecoder =
  decode Spell
    |> required "id" string
    |> required "name" string
    |> required "level" int

wallsDecoder : Decoder Walls
wallsDecoder = 
  decode Walls
    |> required "qty" int
    |> required "level" int


-- UPDATE

loadTownHallDefinition : (Result Http.Error TownHallDefinition -> msg) -> Level -> Cmd msg
loadTownHallDefinition msg level =
  let
    url = "data/town-hall-definitions/town-hall-" ++ (toString level) ++ ".json"
    request = Http.get url townHallDefinitionDecoder  
  in
    Http.send msg request


-- VIEW

townHallLevelSelect : (String -> msg) -> List Level -> Html msg
townHallLevelSelect msg levels =
  let 
    optionize x =
      option [ value (toString x)] 
        [ text (toString x) ]
  in
    label []
      [ text "Change Town Hall Level:"
      , select [ onInput msg ] 
          (option [ disabled True, selected True ] [ text "Select Level" ] :: (List.map optionize levels))
      ]
