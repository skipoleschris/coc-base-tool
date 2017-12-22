module Pallette exposing
  ( Pallette
  , PlacedItem
  , emptyPallette
  , freshPallette
  , currentPalletteItem
  , selectItem
  , changeLevelSelection
  , changeModeSelection
  , refreshPallette
  , viewPallette
  )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, value, disabled)
import Html.Events exposing (onInput, onClick)

import TownHallDefinitions exposing (..)


-- TYPES

type alias Pallette =
  { items : List PalletteItem
  , selected : Maybe String
  , options : PalletteOptions
  , consumptions : Consumptions
  }

type alias PalletteItem =
  { id : String
  , name : String
  , quantity : Int
  , maxLevel : Level
  , minLevel : Maybe Level
  , size : Size 
  , modes : List Mode
  }

type alias PalletteOption =
  { availableLevels : List Level
  , level : Level
  , mode : Maybe String 
  }

type alias PalletteOptions = Dict String PalletteOption

type alias Consumption =
  { numberPlaced : Int
  , modesUsed : Dict String Int
  }

type alias Consumptions = Dics String Consumption

type alias PlacedItem =
  { id : String
  , level : Level
  , mode : Maybe String
  }


-- HELPER FUNCTIONS

emptyPallette : Pallette
emptyPallette = Pallette [] Nothing Dict.empty Dict.empty

freshPallette : TownHallDefinition -> Pallette
freshPallette def = 
  let
    buildings = def.defences ++ def.army ++ def.resources ++ def.traps
    walls = List.map wallsToPalletteItem def.walls
    all = buildings ++ walls
    options = List.map (\i -> (i.id, itemToOption i)) all |> Dict.fromList
  in
    Pallette all Nothing options Dict.empty

wallsToPalletteItem : Walls -> PalletteItem
wallsToPalletteItem w =
  { id = "wall"
  , name = "Wall"
  , quantity = w.quantity
  , maxLevel = w.maxLevel
  , minLevel = Nothing
  , size = Size 1 1
  , modes = []
  }
  
itemToOption : PalletteItem -> PalletteOption
itemToOption item = 
  let
    availableLevels = levelsList item.maxLevel item.minLevel
  in
    { availableLevels = availableLevels
    , level = List.head availableLevels |> Maybe.withDefault 1
    , mode = List.head item.modes |> Maybe.map (\m -> m.id)
    }

levelsList : Level -> Maybe Level -> List Level   
levelsList max maybeMin =
  let
     min = Maybe.withDefault 1 maybeMin
   in
     List.range min max |> List.reverse 

currentPalletteItem : Pallette -> Maybe PlacedItem
currentPalletteItem pallette =
  Maybe.map (\id -> 
    { id = id
    , level = Dict.get id pallette.options |> Maybe.map (\o -> o.level) |> Maybe.withDefault 1
    , mode = Dict.get id pallette.options |> Maybe.map (\o -> o.mode) |> Maybe.withDefault Nothing
    }
  ) pallette.selected 

newConsumption : PlacedItem -> Consumption
newConsumption item =
  let
    modesUsed = item.mode 
                  |> Maybe.map (\mode -> (Dict.fromList [(mode, 1)])) 
                  |> Maybe.withDefault (Dict.fromList [("", 1)])
  in
    { numberPlaced = 1
    , modesUsed = modesUsed
    }

updateConsumption : PlacedItem -> Maybe Consumption -> Maybe Consumption
updateConsumption item consumption =
  case consumption of
    Nothing ->
      newConsumption item
    Just c ->
      { numberPlaced = c.numberPlaced + 1
      , modesUsed = ???
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

refreshPallette : List PlacedItem -> Pallette -> Pallette
refreshPallette placed pallette = pallette


placedItemsToConsumptions : List PlacedItem -> Consumptions
placedItemsToConsumptions items =
  List.foldl updateConsumption Dict.empty items

updateConsumption : PlacedItem -> Consumptions -> Consumptions
updateConsumption item consumptions =
  Dict.update item.id (updateConsumption item) consumptions


-- VIEW

type alias ItemMessage msg = String -> String -> msg

viewPallette : (String -> msg) -> ItemMessage msg -> ItemMessage msg -> Pallette -> Html msg
viewPallette clickMsg levelMsg modeMsg pallette =
  div [ class "pallette" ] (List.map (makePalleteItem clickMsg levelMsg modeMsg pallette.selected pallette.options) pallette.items)

makePalleteItem : (String -> msg) -> ItemMessage msg -> ItemMessage msg -> Maybe String -> PalletteOptions -> PalletteItem -> Html msg
makePalleteItem clickMsg levelMsg modeMsg selectedId options building =
  let
    style = 
      Maybe.map (\id -> if id == building.id then "selected" else "") selectedId |>
      Maybe.withDefault ""
    availableLevels = 
      Dict.get building.id options 
        |> Maybe.map (\opt -> opt.availableLevels)
        |> Maybe.withDefault [1]
  in
    div [ class style ] 
      [ div [ onClick (clickMsg building.id) ] [ text building.name ]
      , div [] [ text ((toString building.quantity) ++ " allowed") ]
      , div [] [ viewLevels (levelMsg building.id) availableLevels ]
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

viewModes : (String -> msg) -> List Mode -> Html msg
viewModes msg modes =
  let
    optionize mode =
      option [ value (mode.id) ] --, disabled (not mode.available) ]      
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
