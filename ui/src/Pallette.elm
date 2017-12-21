module Pallette exposing
  ( Pallette
  , emptyPallette
  , freshPallette
  , selectItem
  , changeLevelSelection
  , changeModeSelection
  , viewPallette
  )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing ( class, value, disabled)
import Html.Events exposing (onInput, onClick)

import TownHallDefinitions exposing (..)


-- TYPES

type alias Pallette =
  { items : List PalletteItem
  , selected : Maybe String
  , options : PalletteOptions
  }

type alias PalletteItem =
  { id : String
  , name : String
  , numberUsed : Int
  , numberRemaining : Int
  , availableLevels : List Level
  , modes : List PalletteMode
  }

type alias PalletteMode = 
  { id : String
  , name : String
  , available : Bool
  }

type alias PalletteOption =
  { level : Level
  , mode : Maybe String 
  }

type alias PalletteOptions = Dict String PalletteOption

-- HELPER FUNCTIONS


emptyPallette : Pallette
emptyPallette = Pallette [] Nothing Dict.empty

freshPallette : TownHallDefinition -> Pallette
freshPallette def = 
  let
    allBuildings = def.defences ++ def.army ++ def.resources ++ def.traps
    b = List.map buildingToPalletteItem allBuildings 
    w = List.map wallsToPalletteItem def.walls
    all = b ++ w
    options = List.map (\i -> (i.id, itemToOption i)) all |> Dict.fromList
  in
    Pallette all Nothing options

buildingToPalletteItem : AllowedBuilding -> PalletteItem
buildingToPalletteItem b = 
  { id = b.id 
  , name = b.name 
  , numberUsed = 0
  , numberRemaining = b.quantity
  , availableLevels = levelsList b.maxLevel b.minLevel
  , modes = List.map modeToPalletteMode b.modes
  }

wallsToPalletteItem : Walls -> PalletteItem
wallsToPalletteItem w =
  { id = "wall"
  , name = "Wall"
  , numberUsed = 0
  , numberRemaining = w.quantity
  , availableLevels = levelsList w.maxLevel Nothing
  , modes = []
  }
  
levelsList : Level -> Maybe Level -> List Level   
levelsList max maybeMin =
  let
     min = Maybe.withDefault 1 maybeMin
   in
     List.range min max |> List.reverse 

modeToPalletteMode : Mode -> PalletteMode
modeToPalletteMode mode =
  { id = mode.id
  , name = mode.name
  , available = True
  }

itemToOption : PalletteItem -> PalletteOption
itemToOption item = 
  { level = List.head item.availableLevels |> Maybe.withDefault 1
  , mode = List.head item.modes |> Maybe.map (\m -> m.id)
  }


-- UPDATE

selectItem : String -> Pallette -> Pallette
selectItem id pallette =
  { pallette | selected = Just id }

changeLevelSelection : String -> Level -> Pallette -> Pallette
changeLevelSelection id level pallette =
  { pallette | 
      options = Dict.update id (Maybe.map (\o -> { o | level = level })) pallette.options
  }

changeModeSelection : String -> String -> Pallette -> Pallette
changeModeSelection id mode pallette =
  { pallette | 
      options = Dict.update id (Maybe.map (\o -> { o | mode = Just mode })) pallette.options
  }


-- VIEW

type alias ItemMessage msg = String -> String -> msg

viewPallette : (String -> msg) -> ItemMessage msg -> ItemMessage msg -> Pallette -> Html msg
viewPallette clickMsg levelMsg modeMsg pallette =
  div [ class "pallette" ] (List.map (makePalleteItem clickMsg levelMsg modeMsg pallette.selected) pallette.items)

makePalleteItem : (String -> msg) -> ItemMessage msg -> ItemMessage msg -> Maybe String -> PalletteItem -> Html msg
makePalleteItem clickMsg levelMsg modeMsg selectedId building =
  let
    style = 
      Maybe.map (\id -> if id == building.id then "selected" else "") selectedId |>
      Maybe.withDefault ""
  in
    div [ class style ] 
      [ div [ onClick (clickMsg building.id) ] [ text building.name ]
      , div [] [ text ((toString building.numberRemaining) ++ " remaining") ]
      , div [] [ viewLevels (levelMsg building.id) building.availableLevels ]
      , div [] [ viewModes (modeMsg building.id) building.modes ]
      ]

viewLevels : (String -> msg) -> List Level -> Html msg
viewLevels msg levels =
  let 
    optionize x =
      option [ value (toString x)] 
        [ text (toString x) ]
  in
    case levels of
      [1] -> 
        text ""
      [x] -> 
        text ("Level: " ++ toString x)
      xs  -> 
        label []
          [ text "Level:"
          , select [ onInput msg ] 
              (List.map optionize xs)
          ]

viewModes : (String -> msg) -> List PalletteMode -> Html msg
viewModes msg modes =
  let
    optionize mode =
      option [ value (mode.id), disabled (not mode.available) ]      
        [ text (mode.name) ]
  in
    case modes of
      [] ->
        text ""
      xs ->
        label []
          [ text "Mode: "
          , select [ onInput msg ] 
              (List.map optionize xs)
          ]
