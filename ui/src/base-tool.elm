import Html exposing (..)
import Html.Attributes exposing (rel, href, class)
import Http exposing (Error)

import TownHallDefinitions exposing (Level, TownHallDefinition, loadTownHallDefinition, townHallLevelSelect)
import Pallette exposing (..)
import Grid exposing (..)

main = 
  Html.program 
    { init = init
    , update = update
    , view = view 
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { grid : Grid
  , townHallLevels : List Level
  , townHallLevel : Maybe Level
  , definition : Maybe TownHallDefinition
  , pallette : Pallette
  , debug : Maybe Error
  }

model : Model
model = 
  { grid = makeGrid defaultSize
  , townHallLevels = [11, 10, 9, 8, 7, 6, 5, 4, 3]
  , townHallLevel = Nothing
  , definition = Nothing
  , pallette = emptyPallette
  , debug = Nothing
  }


-- UPDATE

type Msg = ChangeTownHallLevel String
         | TownHallDefinitionLoaded (Result Http.Error TownHallDefinition)
         | PalletteItemSelected String
         | PalletteLevelChange String String
         | PalletteModeChange String String
         | TileClicked Coordinate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTownHallLevel level ->
      ( { model | 
          townHallLevel = String.toInt level |> Result.toMaybe 
        }
      , String.toInt level 
          |> Result.toMaybe 
          |> Maybe.map (loadTownHallDefinition TownHallDefinitionLoaded)
          |> Maybe.withDefault Cmd.none
      )

    TownHallDefinitionLoaded (Ok definition) ->
      ( { model | 
          definition = Just definition 
        , pallette = freshPallette definition
        }
      , Cmd.none
      )

    TownHallDefinitionLoaded (Err err) ->
      ( { model | 
          townHallLevel = Nothing
        , definition = Nothing
        , pallette = emptyPallette
        , debug = Just err 
        }
      , Cmd.none
      )

    PalletteItemSelected id ->
      ( { model | pallette = selectItem id model.pallette }
      , Cmd.none
      )

    PalletteLevelChange id level ->
      (
        String.toInt level |> Result.toMaybe |> Maybe.map (\lvl ->
          { model | pallette = changeLevelSelection id lvl model.pallette }
        ) |> Maybe.withDefault model
      , Cmd.none
      )

    PalletteModeChange id mode ->  
      ( { model | pallette = changeModeSelection id mode model.pallette }
      , Cmd.none
      )

    TileClicked coordinate ->
      ( updateSelectedTile coordinate model
      , Cmd.none
      )

updateSelectedTile : Coordinate -> Model -> Model
updateSelectedTile coordinate model =
  let
    newGrid = tileSelected coordinate model.grid (currentPalletteItem model.pallette)
    placedItems = allPlacedItems newGrid
    newPallette = refreshPallette placedItems model.pallette
  in
    { model | grid = newGrid, pallette = newPallette }
      

-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ Html.node "link" [ rel "stylesheet", href "dev-styles.css" ] []
    , townHallLevelSelect ChangeTownHallLevel model.townHallLevels
    , viewGrid TileClicked model.grid
    , viewPallette PalletteItemSelected PalletteLevelChange PalletteModeChange model.pallette
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (model, Cmd.none)

