module Toolbar exposing
  ( viewToolbar
  )

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Import exposing (ImportMessage, beginImportWorkflow)

-- VIEW

viewToolbar : msg -> msg -> (ImportMessage -> msg) -> Html msg
viewToolbar clearLayoutMsg exportLayoutMsg importMsg =
  div [ class "toolbar" ]
      [ div [ class "tool-heading" ] [ text "Tools"]
      , div [ class "tool" ] [ button [ onClick clearLayoutMsg ] [ text "Clear Layout" ] ]
      , div [ class "tool" ] [ button [ onClick exportLayoutMsg ] [ text "Export" ] ]
      , div [ class "tool" ] [ button [ onClick (importMsg beginImportWorkflow) ] [ text "Import..." ] ]
      ]
