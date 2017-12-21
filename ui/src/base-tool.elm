import Html exposing (..)
import Html.Attributes exposing (rel, href, class, value, disabled, selected)
import Html.Events exposing (onInput)
import Http exposing (Error)

import TownHallDefinitions exposing (Level, TownHallDefinition, loadTownHallDefinition, townHallLevelSelect)
import Pallette exposing (Pallette, emptyPallette, freshPallette, selectItem, changeLevelSelection, changeModeSelection, viewPallette)

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
  , pallette : Pallette
  , debug : Maybe Error
  }

model : Model
model = 
  { playAreaSize = Dimension 44 44
  , grid = makeGrid (Dimension 44 44)
  , townHallLevels = [11, 10, 9, 8, 7, 6, 5, 4, 3]
  , townHallLevel = Nothing
  , definition = Nothing
  , pallette = emptyPallette
  , debug = Nothing
  }

makeGrid : Dimension -> Grid
makeGrid (Dimension width height) =
  List.repeat height (List.repeat width tile)

tile : Tile
tile =
  {
  }






-- UPDATE

type Msg = ChangeTownHallLevel String
         | TownHallDefinitionLoaded (Result Http.Error TownHallDefinition)
         | PalletteItemSelected String
         | PalletteLevelChange String String
         | PalletteModeChange String String

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


-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ Html.node "link" [ rel "stylesheet", href "dev-styles.css" ] []
    , h2 [] [ text ("Town Hall: " ++ (Maybe.map toString model.townHallLevel |> Maybe.withDefault "-")) ]
    , townHallLevelSelect ChangeTownHallLevel model.townHallLevels
    , viewGrid model.grid
    , viewPallette PalletteItemSelected PalletteLevelChange PalletteModeChange model.pallette
    , div [] [ model.debug |> Maybe.map toString |> Maybe.withDefault "" |> text ]
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

