module LayoutEditor.Overview where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Int (fromString)
import Data.List (List(..), (:), toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Network.HTTP.Affjax as AX

import Model.CoreTypes (Level(..))

type State =
  { townHallLevels :: List Level 
  , townHallLevel :: Maybe Level
  , layoutName :: Maybe String
  , result :: Maybe String
  }

data Query a = LevelChange Level a
             | NameChange String a

type Input = Unit

data Message = LayoutInformation (Maybe Level) (Maybe String)

component :: forall eff. H.Component HH.HTML Query Input Message (Aff (ajax :: AX.AJAX | eff))
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
    { townHallLevels: Level 11 : Level 10 : Level 9 : Level 8 : Level 7 : Level 6 : Level 5 : Level 4 : Level 3 : Nil
    , townHallLevel: Nothing
    , layoutName: Nothing 
    , result: Nothing
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [] 
           [ renderLevelSelect state
           , renderNameEntry state
           , HH.text (fromMaybe "[Not Loaded]" state.result)
           ]

  renderLevelSelect :: State -> H.ComponentHTML Query
  renderLevelSelect state =
    let
      optionise (Level lvl) = 
        HH.option [ HP.value (show lvl) 
                  , HP.selected (Just (Level lvl) == state.townHallLevel) ] 
                  [ HH.text (show lvl) ]

      defaultOption =
        HH.option [ HP.disabled true
                  , HP.selected true ] 
                  [ HH.text "Select Level" ]

      levelInput f = map (HQ.action <<< f <<< Level) <<< fromString
    in 
      HH.div [ HP.class_ (HH.ClassName "town-hall-level") ]
             [ HH.label [] [ HH.text "Town Hall Level: " ] 
             , HH.select [ HE.onValueChange (levelInput LevelChange) ] $ toUnfoldable (defaultOption : map optionise state.townHallLevels)
             ]

  renderNameEntry :: State -> H.ComponentHTML Query
  renderNameEntry state =
    HH.div [ HP.class_ (HH.ClassName "layout-title") ] 
           [ HH.label [] [ HH.text "Layout Title: " ]
           , HH.input [ HP.value (fromMaybe "" state.layoutName)
                      , HP.placeholder "Enter name..."
                      , HE.onValueInput (HE.input NameChange)
                      ]
           ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    LevelChange level next -> do
      state <- H.get
      response <- H.liftAff $ AX.get "/data/town-hall-definitions/town-hall-11.json"
      let nextState = state { townHallLevel = Just level, result = Just response.response }
      H.put nextState
      H.raise $ LayoutInformation nextState.townHallLevel nextState.layoutName
      pure next

    NameChange name next -> do
      state <- H.get
      let nextState = state { layoutName = Just name }
      H.put nextState
      H.raise $ LayoutInformation nextState.townHallLevel nextState.layoutName
      pure next
