import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (rel, href, class)
import Html.Events exposing (onClick)
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

type alias Dimension = 
  { width : Int
  , height : Int
  }

type alias Row = Int
type alias Column = Int
type alias Coordinate = (Row, Column)

type alias Tile = 
  { 
  }

type alias Grid = 
  { tiles : Dict Coordinate Tile
  , size : Dimension
  , lastClicked : Maybe Coordinate
  }

type alias Model =
  { grid : Grid
  , townHallLevels : List Level
  , townHallLevel : Maybe Level
  , definition : Maybe TownHallDefinition
  , pallette : Pallette
  , debug : Maybe Error
  }

defaultSize : Dimension 
defaultSize = { width = 44, height = 44 }

model : Model
model = 
  { grid = makeGrid defaultSize
  , townHallLevels = [11, 10, 9, 8, 7, 6, 5, 4, 3]
  , townHallLevel = Nothing
  , definition = Nothing
  , pallette = emptyPallette
  , debug = Nothing
  }

makeGrid : Dimension -> Grid
makeGrid dimension =
  let
    tiles = List.map (\c -> (c, tile)) (allCoordinates dimension) 
  in
    { tiles = Dict.fromList tiles
    , size = dimension
    , lastClicked = Nothing
    }

allCoordinates : Dimension -> List Coordinate
allCoordinates size =
  let
    colRange = List.range 1 size.width
    rowRange = List.range 1 size.height
  in
    List.map (\r -> List.map (\c -> (r, c)) colRange) rowRange |> List.concat

rowIndexes : Grid -> List Row
rowIndexes grid =
  List.range 1 grid.size.height

colIndexes : Grid -> List Column
colIndexes grid =
  List.range 1 grid.size.width

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
      ( { model | 
          grid = tileSelected coordinate model.grid model.pallette 
        , pallette = refreshPallette [] model.pallette
        }
      , Cmd.none
      )

tileSelected : Coordinate -> Grid -> Pallette -> Grid
tileSelected coordinate grid pallette =
  { grid | lastClicked = Just coordinate }

type alias PlacedBuilding = {}

refreshPallette : List PlacedBuilding -> Pallette -> Pallette
refreshPallette placed pallette = pallette

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
  div [ class "map" ] (List.map (makeRow grid) (rowIndexes grid))

makeRow : Grid -> Row -> Html Msg
makeRow grid row =
  div [ class "row" ] (List.map (makeCol grid row) (colIndexes grid))

makeCol : Grid -> Row -> Column -> Html Msg
makeCol grid row col =
  div [ class "tile grass", onClick (TileClicked (row, col)) ] []




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (model, Cmd.none)

