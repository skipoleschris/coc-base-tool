module LayoutEditor.Pallette where

import Prelude (type (~>), bind, const, discard, map, not, pure, show, ($), (&&), (<<<), (<>), (==))

import Data.String.Utils (startsWith)
import Data.Array as Array
import Data.Int (fromString)
import Data.List as List
import Data.List ((:), toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ

import Model.CoreTypes (Level(..))
import Model.TownHallDefinitions (AllowedBuilding, Mode(..))
import Model.ItemsSelector (ItemSelector, changeLevelSelection, changeModeSelection, emptySelector, selectItem, selectableBuildings, BuildingInfo, buildingInfo)

type State = {
  selector :: ItemSelector
}

type Input = ItemSelector

data Query a = ItemSelected String a
             | LevelChange String Level a
             | ModeChange String String a
             | ReplaceSelector ItemSelector a

data Message = SelectorUpdate ItemSelector

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input ReplaceSelector
    }
  where

  initialState :: State
  initialState = {
    selector: emptySelector
  }

  render :: State -> H.ComponentHTML Query
  render state =
    let
      visibleItems = selectableBuildings state.selector
    in
      HH.div [ HP.class_ (HH.ClassName "pallette") ] 
             (Array.fromFoldable $ map (makePalletteItem state.selector) visibleItems)

  makePalletteItem :: ItemSelector -> AllowedBuilding -> H.ComponentHTML Query
  makePalletteItem selector building =
    let
      info = buildingInfo building selector

      style = if info.isSelected then " selected" else "" 
    in
      HH.div [ HP.class_ (HH.ClassName ("pallete-item" <> style)) 
             , HE.onClick (HE.input_ (ItemSelected info.id))
             ]
             [ HH.div [ HP.class_ (HH.ClassName "pallette-image") ]
                      [ HH.img [ HP.src (buildingImage info) 
                               , HP.alt info.name
                               ]
                      ]
             , HH.div [ HP.class_ (HH.ClassName "pallette-detail") ]
                      [ HH.text info.name
                      , HH.br_
                      , HH.text (show info.placedCount <> " of " <> show info.quantity <> " placed")
                      ]
             , HH.div []
                      [ viewLevels info
                      , viewModes info 
                      ]
             ]         

  buildingImage :: BuildingInfo -> String
  buildingImage info = 
    let 
      id = 
        if (startsWith "wall" info.id)
        then "wall"
        else info.id

      level = 
        info.level

      mode = 
        (fromMaybe "" <<< map (\m -> "-" <> m)) $ info.mode
    in
      "data/images/" <> 
      id <>
      "/pallette/" <>
      id <> 
      "-" <> 
      show level <> 
      mode <>
      ".png"      

  viewLevels :: BuildingInfo -> H.ComponentHTML Query
  viewLevels info =
    let
      optionise lvl = 
        HH.option [ HP.value (show lvl) 
                  , HP.selected (info.level == lvl) 
                  ] 
                  [ HH.text (show lvl) ]

      levelInput f = map (HQ.action <<< f <<< Level) <<< fromString
    in                  
      case info.levels of
        Level 1 : List.Nil ->
          HH.text ""

        (Level level) : List.Nil ->
          HH.text ("Level: " <> show level)

        xs ->
          HH.div []
                 [ HH.div [ HP.class_ (HH.ClassName "pallette-label") ] 
                          [ HH.label [] [ HH.text "Level: " ] ]
                 , HH.select [ HE.onValueChange (levelInput (LevelChange info.id))
                  ] $ toUnfoldable (map optionise xs)
                 ]

  viewModes :: BuildingInfo -> H.ComponentHTML Query
  viewModes info = 
    let 
      optionise (Mode mode) = 
        HH.option [ HP.value mode.id 
                  , HP.selected (isSelected mode && not (isDisabled mode))
                  , HP.disabled (isDisabled mode)
                  ] 
                  [ HH.text mode.name ]

      modeInput f = map (HQ.action <<< f) <<< Just
    in
      case info.modes of
        List.Nil ->
          HH.text ""

        xs ->
          HH.div []
                 [ HH.div [ HP.class_ (HH.ClassName "pallette-label") ] 
                          [ HH.label [] [ HH.text "Mode: " ] ]
                 , HH.select [ HE.onValueChange (modeInput (ModeChange info.id))
                  ] $ toUnfoldable (map optionise xs)
                 ]
    where
      isSelected mode = Just mode.id == info.mode
      isDisabled mode = info.isModeDisabled mode.id

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ItemSelected id next -> do
      state <- H.get
      let nextState = { selector: selectItem id state.selector }
      H.put nextState
      H.raise $ SelectorUpdate nextState.selector
      pure next

    LevelChange id level next -> do
      state <- H.get
      let nextState = { selector: changeLevelSelection id level state.selector }
      H.put nextState
      H.raise $ SelectorUpdate nextState.selector
      pure next

    ModeChange id mode next -> do
      state <- H.get
      let nextState = { selector: changeModeSelection id mode state.selector }
      H.raise $ SelectorUpdate nextState.selector
      pure next

    ReplaceSelector selector next -> do
      H.put { selector: selector }
      pure next
