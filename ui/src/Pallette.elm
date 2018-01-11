module Pallette exposing
  ( Pallette
  , PlacedItem
  , emptyPallette
  , freshPallette
  , currentPalletteItem
  , itemSize
  , isItemAvailable
  , selectItem
  , changeLevelSelection
  , changeModeSelection
  , refreshPallette
  , viewPallette
  )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, value, disabled, selected, src, alt)
import Html.Events exposing (onInput, onClick)

import Common exposing (..)
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
  , modes : List PalletteMode
  }

type alias PalletteMode =
  { id : String
  , name : String
  , maxAllowed : Maybe Int 
  , minLevel : Maybe Int
  }

type alias PalletteOption =
  { availableLevels : List Level
  , disabledModes : List String
  , lockedModes : List String
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
  , size : Size
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
  { id = "wall" ++ (toString w.maxLevel)
  , name = "Wall (max level " ++ (toString w.maxLevel) ++ ")"
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
    , lockedModes = []
    , level = List.head availableLevels |> Maybe.withDefault 1
    , mode = List.head item.modes |> Maybe.map (\m -> m.id)
    }

levelsList : Level -> Maybe Level -> List Level   
levelsList max maybeMin =
  let
     min = Maybe.withDefault 1 maybeMin
   in
     List.range min max |> List.reverse 

updateOptionsFromConsumptions : List PalletteItem -> Consumptions -> PalletteOptions -> PalletteOptions
updateOptionsFromConsumptions items consumptions options =
  items
    |> List.map (updateOptionsForItem consumptions options)
    |> Dict.fromList

updateOptionsForItem : Consumptions -> PalletteOptions -> PalletteItem -> (String, PalletteOption)
updateOptionsForItem consumptions options item =
  let
    option = 
      options
        |> Dict.get item.id
        |> Maybe.withDefault (itemToOption item)
      
    consumption =
      Dict.get item.id consumptions
  in
    case consumption of
      Nothing ->
        (item.id, option)
      Just c ->
        (item.id, applyOptionUpdate item.modes c option)

applyOptionUpdate : List PalletteMode -> Consumption -> PalletteOption -> PalletteOption
applyOptionUpdate modes consumption option = 
  let
    consumedModes =
      modes 
        |> List.filter (isModeConsumed consumption)
        |> List.map (\m -> m.id)
      
    invalidMode = 
      option.mode
        |> Maybe.map (\m -> List.member m consumedModes)
        |> Maybe.withDefault False

    defaultMode =
      modes
        |> List.head
        |> Maybe.map (\m -> m.id)
  in
      { option | 
        disabledModes = consumedModes 
      , mode = if invalidMode then defaultMode else option.mode 
      }

isModeConsumed : Consumption -> PalletteMode -> Bool  
isModeConsumed consumption mode =
  let
    numberPlaced =
      consumption.modesUsed
        |> Dict.get mode.id
        |> Maybe.withDefault 0
  in
    mode.maxAllowed
      |> Maybe.map (\max -> numberPlaced >= max)
      |> Maybe.withDefault False
      
currentPalletteItem : Pallette -> Maybe PlacedItem
currentPalletteItem pallette =
  Maybe.map (\id -> 
    { id = id
    , level = Dict.get id pallette.options |> Maybe.map (\o -> o.level) |> Maybe.withDefault 1
    , mode = Dict.get id pallette.options |> Maybe.map (\o -> o.mode) |> Maybe.withDefault Nothing
    , size = itemSize pallette id
    }
  ) pallette.selected 

itemSize : Pallette -> String -> Size
itemSize pallette id =
  pallette.items
    |> List.filter (\i -> i.id == id)
    |> List.head
    |> Maybe.map (\i -> i.size)
    |> Maybe.withDefault { width = 1, height = 1 }

newConsumption : PlacedItem -> Consumption
newConsumption item =
  let
    modesUsed = item.mode 
                  |> Maybe.map (\mode -> (Dict.fromList [(mode, 1)])) 
                  |> Maybe.withDefault Dict.empty
  in
    { numberPlaced = 1
    , modesUsed = modesUsed
    }

updateConsumptionWithItem : PlacedItem -> Maybe Consumption -> Maybe Consumption
updateConsumptionWithItem item consumption =
  case consumption of
    Nothing ->
      Just (newConsumption item)
    Just c ->
      Just (applyAdditionalConsumption item c)

applyAdditionalConsumption : PlacedItem -> Consumption -> Consumption
applyAdditionalConsumption item consumption =
  let
    numberPlaced = 
      consumption.numberPlaced + 1

    modesUsed =
      updateModesUsed item.mode consumption.modesUsed
  in
    { numberPlaced = numberPlaced
    , modesUsed = modesUsed
    }

updateModesUsed : Maybe String -> Dict String Int -> Dict String Int
updateModesUsed mode used =
  case mode of
    Nothing ->
      used
    Just m ->
      Dict.update m incrementCount used

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

isItemAvailable : Pallette -> PlacedItem -> Bool
isItemAvailable pallette item = True

-- UPDATE

selectItem : String -> Pallette -> Pallette
selectItem id pallette =
  { pallette | selected = Just id }

changeLevelSelection : String -> Level -> Pallette -> Pallette
changeLevelSelection id level pallette =
  let
    modes = 
      pallette.items
        |> List.filter (\i -> i.id == id)
        |> List.head
        |> Maybe.map (\i -> i.modes)
        |> Maybe.withDefault []
  in
    { pallette | 
        options = Dict.update id (Maybe.map (updateOptionLevel modes level)) pallette.options
    }

updateOptionLevel : List PalletteMode -> Level -> PalletteOption -> PalletteOption  
updateOptionLevel modes newLevel option =
  let
    lockedModes = 
      identifyLockedModes modes newLevel

    newMode =
      case option.mode of
        Nothing -> 
          Nothing
        Just m ->
          if (List.member m lockedModes)
          then List.head modes |> Maybe.map (\m -> m.id)
          else Just m
  in
    {
      option | 
      level = newLevel
    , mode = newMode
    , lockedModes = lockedModes
    }

identifyLockedModes : List PalletteMode -> Level -> List String
identifyLockedModes modes level =
  modes 
    |> List.filter (\m -> 
         case m.minLevel of
           Nothing -> False
           Just l  -> l > level
       )
    |> List.map (\m -> m.id)

changeModeSelection : String -> String -> Pallette -> Pallette
changeModeSelection id mode pallette =
  { pallette | 
      options = Dict.update id (Maybe.map (\o -> { o | mode = Just mode })) pallette.options
  }

refreshPallette : List PlacedItem -> Pallette -> Pallette
refreshPallette placed pallette = 
  let
    consumptions = 
      placedItemsToConsumptions placed

    options =
      updateOptionsFromConsumptions pallette.items consumptions pallette.options

    selected =
      case pallette.selected of
        Nothing -> 
          Nothing
        Just id ->
          pallette.items          
            |> List.filter (\i -> i.id == id)
            |> List.head
            |> Maybe.andThen (\i -> 
                if isNotConsumed consumptions i
                then Just id
                else Nothing
              )
  in   
    { pallette | 
      consumptions = consumptions
    , options = options
    , selected = selected
    }

placedItemsToConsumptions : List PlacedItem -> Consumptions
placedItemsToConsumptions items =
  List.foldl updateConsumption Dict.empty items

updateConsumption : PlacedItem -> Consumptions -> Consumptions
updateConsumption item consumptions =
    Dict.update item.id (updateConsumptionWithItem item) consumptions


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
    div [ class ("pallette-item " ++ style) 
        , onClick (clickMsg item.id)
        ] 
        [ div [ class "pallette-image" ] 
              [ img [ src (itemImage item option)
                    , alt item.name
                    ] [] 
              ]
        , div [ class "pallette-detail" ] 
              [ text item.name 
              , br [] []
              , text ((toString placedCount) ++ " of " ++ (toString item.quantity) ++ " placed")
              ]
        , div [] 
              [ viewLevels (levelMsg item.id) option availableLevels
              , viewModes (modeMsg item.id) option item.modes
              ]
        ]

itemImage : PalletteItem -> Maybe PalletteOption -> String
itemImage item option =
  let 
    id = 
      if (String.startsWith "wall" item.id)
      then "wall"
      else item.id

    level = 
      option
        |> Maybe.map (\o -> o.level)
        |> Maybe.withDefault 1

    mode =
      option 
        |> Maybe.andThen (\o -> o.mode)
        |> Maybe.map (\m -> "-" ++ m)
        |> Maybe.withDefault ""
  in
    "data/images/" ++ 
    id ++ 
    "/pallette/" ++ 
    id ++ 
    "-" ++ 
    (toString level) ++ 
    mode ++
    ".png"      

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
        div []
            [ div [ class "pallette-label" ] [ label [] [ text "Level:" ] ]
            , select [ onInput msg, class "pallette-select" ] (List.map optionize xs)
            ]

isLevelSelected : Level -> Maybe PalletteOption -> Bool
isLevelSelected level option =
  Maybe.map (\opt -> opt.level == level) option 
    |> Maybe.withDefault False

viewModes : (String -> msg) -> Maybe PalletteOption -> List PalletteMode -> Html msg
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
        div []
            [ div [ class "pallette-label" ] [ label [] [ text "Mode: " ] ]
            , select [ onInput msg, class "pallette-select" ] (List.map optionize xs)
            ]

isModeSelected : PalletteMode -> Maybe PalletteOption -> Bool
isModeSelected mode option =
  Maybe.map (\opt -> opt.mode == Just mode.id) option 
    |> Maybe.withDefault False

isModeDisabled : PalletteMode -> Maybe PalletteOption -> Bool
isModeDisabled mode option = 
  option 
    |> Maybe.map (\opt -> 
          List.member mode.id opt.disabledModes ||
          List.member mode.id opt.lockedModes
       )
    |> Maybe.withDefault False
