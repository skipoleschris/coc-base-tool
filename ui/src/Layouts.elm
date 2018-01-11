module Layouts exposing
  ( Layout
  , emptyLayout
  , newLayout
  , changeLayoutName
  , changeTownHallLevel
  , viewLayout
  )

import Html exposing (..)
import Html.Attributes exposing (value, disabled, selected, class, size, placeholder)
import Html.Events exposing (onInput)

import Common exposing (Level)


-- TYPES

type alias Layout = 
  { townHallLevels : List Level
  , townHallLevel : Maybe Level
  , layoutName : Maybe String
  }


-- MODEL

emptyLayout : Layout
emptyLayout =
  { townHallLevels = [11, 10, 9, 8, 7, 6, 5, 4, 3]
  , townHallLevel = Nothing
  , layoutName = Nothing
  }


-- UPDATE

newLayout : Level -> Maybe String -> Layout
newLayout townHallLevel layoutName =
  { emptyLayout | townHallLevel = Just townHallLevel
                , layoutName = layoutName }

changeLayoutName : String -> Layout -> Layout
changeLayoutName newName layout =
  { layout | layoutName = if newName == "" then Nothing else Just newName }

changeTownHallLevel : String -> (Level -> Cmd msg) -> Cmd msg 
changeTownHallLevel level msg =
  String.toInt level 
          |> Result.toMaybe 
          |> Maybe.map msg
          |> Maybe.withDefault Cmd.none

-- VIEW

viewLayout : (String -> msg) -> (String -> msg) -> Layout -> Html msg
viewLayout selectMsg titleMsg layout =
  div [] [ townHallLevelSelect selectMsg layout
         , layoutTitle titleMsg layout
         ]

townHallLevelSelect : (String -> msg) -> Layout -> Html msg
townHallLevelSelect msg layout =
  let 
    optionize x =
      option [ value (toString x)
             , selected (Just x == layout.townHallLevel)
             ] 
        [ text (toString x) ]
  in
    div [ class "town-hall-level" ] [
      label []
        [ text "Town Hall Level: "
        , select [ onInput msg ] 
            (option [ disabled True, selected True ] 
                    [ text "Select Level" ] :: (List.map optionize layout.townHallLevels))
        ]
    ]

layoutTitle : (String -> msg) -> Layout -> Html msg
layoutTitle msg layout =
  let
    name =
      Maybe.withDefault "" layout.layoutName
  in
      
  div [ class "layout-title" ]
      [ label [] [ text "Layout Title: " ]
      , input [ size 50
              , value name
              , placeholder "Enter name..."
              , onInput msg ] 
              []
      ]

