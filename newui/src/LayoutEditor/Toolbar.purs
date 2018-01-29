module LayoutEditor.Toolbar where

import Prelude
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { wallDrawingMode :: Boolean 
  }

data Query a = ClearLayoutClicked a
             | ExportClicked a
             | ImportClicked a
             |  ToggleWallDrawingMode Boolean a

type Input = Unit

data Message = ClearLayout
             | ExportLayout
             | ImportLayout
             | WallDrawingMode Boolean

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 
    { wallDrawingMode: true 
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "toolbar") ] 
           [ HH.div [ HP.class_ (HH.ClassName "tool-heading") ] 
                    [ HH.text "Tools" ]
           , HH.div [ HP.class_ (HH.ClassName "tool") ] 
                    [ HH.button [ HE.onClick (HE.input_ ClearLayoutClicked) ] [ HH.text "Clear Layout" ] ]
           , HH.div [ HP.class_ (HH.ClassName "tool") ] 
                    [ HH.button [ HE.onClick (HE.input_ ExportClicked) ] [ HH.text "Export" ] ]
           , HH.div [ HP.class_ (HH.ClassName "tool") ] 
                    [ HH.button [ HE.onClick (HE.input_ ImportClicked) ] [ HH.text "Import..." ] ]
           , HH.div [ HP.class_ (HH.ClassName "tool") ] 
                    [ wallDrawingModeCheckbox state ]
           ]

  wallDrawingModeCheckbox :: State -> H.ComponentHTML Query
  wallDrawingModeCheckbox state =
    HH.div []
           [ HH.label [] [ HH.text "Wall drawing mode" ]
           , HH.input [ HP.type_ HP.InputCheckbox 
                      , HP.checked state.wallDrawingMode
                      , HE.onChecked (HE.input ToggleWallDrawingMode)
                      ]
           ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ClearLayoutClicked next -> do
      H.raise ClearLayout
      pure next

    ExportClicked next -> do
      H.raise ExportLayout
      pure next

    ImportClicked next -> do
      H.raise ImportLayout
      pure next

    ToggleWallDrawingMode mode next -> do
      state <- H.get
      let nextState = state { wallDrawingMode = mode }
      H.put nextState
      H.raise $ WallDrawingMode nextState.wallDrawingMode
      pure next


