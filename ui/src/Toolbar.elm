module Toolbar exposing
  ( ToolbarState
  , ToolbarMessage
  , initialToolbar
  , updateToolbar
  ,  viewToolbar
  )

import Html exposing (..)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onClick)

import Import exposing (ImportMessage, beginImportWorkflow)


-- TYPES

type alias ToolbarState =
  { wallDrawingMode : Bool
  }

type ToolbarMessage = ToggleWallDrawingMode


-- MODEL

initialToolbar : ToolbarState
initialToolbar = 
  { wallDrawingMode = True }


-- UPDATE

updateToolbar : ToolbarMessage -> ToolbarState -> ToolbarState
updateToolbar msg state =
  case msg of
    ToggleWallDrawingMode -> { state | wallDrawingMode = not state.wallDrawingMode }


-- VIEW

viewToolbar : msg -> msg -> (ImportMessage -> msg) -> (ToolbarMessage -> msg) -> ToolbarState -> Html msg
viewToolbar clearLayoutMsg exportLayoutMsg importMsg toolbarMsg state =
  div [ class "toolbar" ]
      [ div [ class "tool-heading" ] [ text "Tools"]
      , div [ class "tool" ] [ button [ onClick clearLayoutMsg ] [ text "Clear Layout" ] ]
      , div [ class "tool" ] [ button [ onClick exportLayoutMsg ] [ text "Export" ] ]
      , div [ class "tool" ] [ button [ onClick (importMsg beginImportWorkflow) ] [ text "Import..." ] ]
      , div [ class "tool" ] [ checkbox (toolbarMsg ToggleWallDrawingMode) "Wall drawing mode" state.wallDrawingMode ]
      ]

checkbox : msg -> String -> Bool -> Html msg
checkbox msg name mode =
  div [] 
      [ label [] [ text name ] 
      , input [ type_ "checkbox", checked mode, onClick msg ] []
      ]
