module Toolbar exposing
  ( viewToolbar
  )

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- VIEW

viewToolbar : msg -> Html msg
viewToolbar clearLayoutMsg =
  div [ class "toolbar" ]
      [ div [ class "tool-heading" ] [ text "Tools"]
      , div [ class "tool" ] [ button [ onClick clearLayoutMsg ] [ text "Clear Layout" ] ]
      , div [ class "tool" ] [ button [] [ text "Export..." ] ]
      , div [ class "tool" ] [ button [] [ text "Import..." ] ]
      ]
