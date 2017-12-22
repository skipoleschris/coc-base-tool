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
import Html.Attributes exposing (class, value, disabled, selected)
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
  , disabledModes : List String
  , level : Level
  , mode : Maybe String 
  }

type alias PalletteOptions = Dict String PalletteOption

type alias Consumption =
  { numberPlaced : Int
  , modesUsed : Dict String Int
  }

type alias Consumptions = Dict String Consumption

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
    availableLevels = 
      levelsList item.maxLevel item.minLevel
  in
    { availableLevels = availableLevels
    , disabledModes = []
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

newConsumption : PlacedItem -> List Mode -> Consumption
newConsumption item modes =
  let
    modesUsed = item.mode 
                  |> Maybe.map (\mode -> (Dict.fromList [(mode, 1)])) 
                  |> Maybe.withDefault Dict.empty

    modesDisabled = item.mode
                      |> Maybe.map (\mode -> if (isModeConsumed mode 1 modes) then [mode] else [])
                      |> Maybe.withDefault []
  in
    { numberPlaced = 1
    , modesUsed = modesUsed
    , modesDisabled = modesDisabled
    }

isModeConsumed : String -> Int -> List Mode -> Bool
isModeConsumed mode numberPlaced modes = 
  let
    maxAllowed = modes
                   |> List.filter (\m -> m.id == mode)
                   |> List.head
                   |> Maybe.andThen (\m -> m.maxAllowed)
    
  in
    maxAllowed
      |> Maybe.map (\max -> numberPlaced >= max)
      |> Maybe.withDefault False

updateConsumptionWithItem : PlacedItem -> List Mode -> Maybe Consumption -> Maybe Consumption
updateConsumptionWithItem item modes consumption =
  case consumption of
    Nothing ->
      Just (newConsumption item modes)
    Just c ->
      Just (applyAdditionalConsumption item modes c)

applyAdditionalConsumption : PlacedItem -> List Mode -> Consumption -> Consumption
applyAdditionalConsumption item modes consumption =
  let
    numberPlaced = 
      consumption.numberPlaced + 1

    modesUsed =
      updateModesUsed item.mode consumption.modesUsed
  in
    { numberPlaced = numberPlaced
    , modesUsed = modesUsed
    , modesDisabled = updateModesDisabled modesUsed modes
    }

updateModesUsed : Maybe String -> Dict String Int -> Dict String Int
updateModesUsed mode used =
  case mode of
    Nothing ->
      used
    Just m ->
      Dict.update m incrementCount used

updateModesDisabled : Dict String Int -> List Mode -> List String
updateModesDisabled modesUsed modes = 
  modesUsed 
    |> Dict.toList
    |> List.filter (\mode -> isModeConsumed (Tuple.first mode) (Tuple.second mode) modes)
    |> List.map (\mode -> Tuple.first mode)

incrementCount : Maybe Int -> Maybe Int
incrementCount count = 
  case count of
    Nothing -> Just 1
    Just c -> Just (c + 1)

numberConsumed : Consumptions -> String -> Int
numberConsumed consumptions id =
  Dict.get id consumptions 
    |> Maybe.map (\c -> c.numberPlaced)
    |> Maybe.withDefault 0    

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
refreshPallette placed pallette = 
  { pallette | consumptions = placedItemsToConsumptions pallette.items placed }

placedItemsToConsumptions : List PalletteItem -> List PlacedItem -> Consumptions
placedItemsToConsumptions palletteItems items =
  List.foldl (updateConsumption palletteItems) Dict.empty items

updateConsumption : List PalletteItem -> PlacedItem -> Consumptions -> Consumptions
updateConsumption palletteItems item consumptions =
  let
    modes = palletteItems
              |> List.filter (\i -> i.id == item.id)
              |> List.head
              |> Maybe.map (\i -> i.modes)
              |> Maybe.withDefault []
  in
    Dict.update item.id (updateConsumptionWithItem item modes) consumptions


-- VIEW

type alias ItemMessage msg = String -> String -> msg

viewPallette : (String -> msg) -> ItemMessage msg -> ItemMessage msg -> Pallette -> Html msg
viewPallette clickMsg levelMsg modeMsg pallette =
  div [ class "pallette" ] 
    (List.map (makePalleteItem clickMsg levelMsg modeMsg pallette) 
              (removeConsumedItems pallette))

removeConsumedItems : Pallette -> List PalletteItem
removeConsumedItems pallette =
  List.filter (isNotConsumed pallette.consumptions) pallette.items 

isNotConsumed : Consumptions -> PalletteItem -> Bool
isNotConsumed consumptions item =
  (numberConsumed consumptions item.id) < item.quantity

makePalleteItem : (String -> msg) -> ItemMessage msg -> ItemMessage msg -> Pallette -> PalletteItem -> Html msg
makePalleteItem clickMsg levelMsg modeMsg pallette item =
  let
    style = 
      Maybe.map (\id -> if id == item.id then "selected" else "") pallette.selected |>
      Maybe.withDefault ""

    availableLevels = 
      Dict.get item.id pallette.options 
        |> Maybe.map (\opt -> opt.availableLevels)
        |> Maybe.withDefault [1]

    placedCount = 
      numberConsumed pallette.consumptions item.id

    option = 
      Dict.get item.id pallette.options
  in
    div [ class style ] 
      [ div [ onClick (clickMsg item.id) ] [ text item.name ]
      , div [] [ text ((toString placedCount) ++ " of " ++ (toString item.quantity) ++ " placed") ]
      , div [] [ viewLevels (levelMsg item.id) option availableLevels ]
      , div [] [ viewModes (modeMsg item.id) option item.modes ]
      ]

viewLevels : (String -> msg) -> Maybe PalletteOption -> List Level -> Html msg
viewLevels msg opt levels =
  let 
    optionize x =
      option [ value (toString x), selected (isLevelSelected x opt)] 
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

isLevelSelected : Level -> Maybe PalletteOption -> Bool
isLevelSelected level option =
  Maybe.map (\opt -> opt.level == level) option 
    |> Maybe.withDefault False

viewModes : (String -> msg) -> Maybe PalletteOption -> List Mode -> Html msg
viewModes msg opt modes =
  let
    optionize mode =
      option [ value (mode.id)
             , selected (isModeSelected mode opt && (not (isModeDisabled mode opt)))
             , disabled (isModeDisabled mode opt) 
             ]      
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

isModeSelected : Mode -> Maybe PalletteOption -> Bool
isModeSelected mode option =
  Maybe.map (\opt -> opt.mode == Just mode.id) option 
    |> Maybe.withDefault False

isModeDisabled : Mode -> Maybe PalletteOption -> Bool
isModeDisabled mode option = 
  option 
    |> Maybe.map (\opt -> List.member mode.id opt.modesDisabled)
    |> Maybe.withDefault False
