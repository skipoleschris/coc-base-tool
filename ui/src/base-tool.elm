import Html exposing (..)
import Html.Attributes exposing (rel, href, class, value, disabled, selected)
import Html.Events exposing (onInput)
import List exposing (repeat) 
import Maybe exposing (..)

import Http exposing (get, send, Error)
--import Json.Decode exposing (int, string, nullable, list, Decoder)
--import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

import TownHallDefinitions exposing (..)

main = 
  Html.program 
    { init = init
    , update = update
    , view = view 
    , subscriptions = subscriptions
    }


-- MODEL

type Dimension = Dimension Int Int


type alias Tile = 
  {
  }

type alias Grid = List (List Tile)

type alias Model =
  { playAreaSize : Dimension
  , grid : Grid
  , townHallLevels : List Level
  , townHallLevel : Maybe Level
  , definition : Maybe TownHallDefinition
  , debug : Maybe Error
  }

model : Model
model = 
  { playAreaSize = Dimension 44 44
  , grid = makeGrid (Dimension 44 44)
  , townHallLevels = [11, 10, 9, 8, 7, 6, 5, 4, 3]
  , townHallLevel = Nothing
  , definition = Nothing
  , debug = Nothing
  }

makeGrid : Dimension -> Grid
makeGrid (Dimension width height) =
  repeat height (repeat width tile)

tile : Tile
tile =
  {
  }





-- UPDATE

type Msg = ChangeTownHallLevel String
         | TownHallDefinitionLoaded (Result Http.Error TownHallDefinition)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTownHallLevel level ->
      ({ model | townHallLevel = String.toInt level 
          |> Result.toMaybe 
       }, 
       String.toInt level 
         |> Result.toMaybe 
         |> Maybe.map loadTownHallDefinition 
         |> Maybe.withDefault Cmd.none)

    TownHallDefinitionLoaded (Ok definition) ->
      ({ model | definition = Just definition }, Cmd.none)

    TownHallDefinitionLoaded (Err err) ->
      ({ model | townHallLevel = Nothing, definition = Nothing, debug = Just err }, Cmd.none) -- TODO


loadTownHallDefinition : Level -> Cmd Msg
loadTownHallDefinition level =
  let
    url = "data/town-hall-definitions/town-hall-" ++ (toString level) ++ ".json"
    request = Http.get url townHallDefinitionDecoder  
  in
    Http.send TownHallDefinitionLoaded request

-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ Html.node "link" [ rel "stylesheet", href "dev-styles.css" ] []
    , h2 [] [ text ("Town Hall: " ++ (withDefault "-" (Maybe.map toString model.townHallLevel))) ]
    , viewLevelSelect "Change Town Hall Level:" ChangeTownHallLevel model.townHallLevels
    , viewGrid model.grid
    , div [] [ model.debug |> Maybe.map toString |> Maybe.withDefault "" |> text ]
    ]

viewLevelSelect : String -> (String -> msg) -> List Level -> Html msg
viewLevelSelect lbl msg levels =
  let 
    optionize x =
      option [ value (toString x)] 
        [ text (toString x) ]
  in
    label []
      [ text lbl
      , select [ onInput msg ] (option [ disabled True, selected True ] [ text "Select Level" ] :: (List.map optionize levels))
      ]
  

viewGrid : Grid -> Html Msg
viewGrid grid =
  div [ Html.Attributes.class "map" ] (List.map makeRow grid)

makeRow : List Tile -> Html Msg
makeRow tiles =
  div [ Html.Attributes.class "row" ] (List.map makeTile tiles)

makeTile : Tile -> Html Msg
makeTile tile =
  div [ Html.Attributes.class "tile grass" ] []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (model, Cmd.none)

