module LayoutEditor.Pallette where

import Prelude (type (~>), bind, const, discard, map, not, pure, show, ($), (&&), (<<<), (<>), (==), (||))

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

import Model.CoreTypes (Level(..), PlacedItem)
import Model.TownHallDefinitions (TownHallDefinition(..), AllowedBuilding(..), Mode(..))
import Model.ItemsSelector (ItemSelector, Option(..), availableLevels, changeLevelSelection, changeModeSelection, emptySelector, freshSelector, getBuildingOption, isSelected, numberConsumed, selectItem, selectableBuildings, currentlySelected)

type State =
  { selector :: ItemSelector
  , townHallLevel :: Maybe Level
  }

data Query a = ItemSelected String a
             | LevelChange String Level a
             | ModeChange String String a
             | SelectedTownHallDefinition (Maybe TownHallDefinition) a

type Input = Maybe TownHallDefinition

data Message = PalletteSelection (Maybe PlacedItem)

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input SelectedTownHallDefinition
    }
  where

  initialState :: State
  initialState = 
    { selector: emptySelector 
    , townHallLevel: Nothing
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
      (AllowedBuilding b) = building

      style = 
        if (isSelected selector building) 
        then " selected" 
        else "" 

      levels = 
        availableLevels selector building

      placedCount = 
        numberConsumed selector building

      option =
        getBuildingOption selector building
    in
      HH.div [ HP.class_ (HH.ClassName ("pallete-item" <> style)) 
             , HE.onClick (HE.input_ (ItemSelected b.id))
             ]
             [ HH.div [ HP.class_ (HH.ClassName "pallette-image") ]
                      [ HH.img [ HP.src (buildingImage building option) 
                               , HP.alt b.name
                               ]
                      ]
             , HH.div [ HP.class_ (HH.ClassName "pallette-detail") ]
                      [ HH.text b.name
                      , HH.br_
                      , HH.text (show placedCount <> " of " <> show b.quantity <> " placed")
                      ]
             , HH.div []
                      [ viewLevels b.id option levels
                      , viewModes b.id option b.modes 
                      ]
             ]         

  buildingImage :: AllowedBuilding -> Option -> String
  buildingImage (AllowedBuilding building) (Option option) = 
    let 
      id = 
        if (startsWith "wall" building.id)
        then "wall"
        else building.id

      level = 
        option.level

      mode = 
        (fromMaybe "" <<< map (\m -> "-" <> m)) $ option.mode
    in
      "data/images/" <> 
      id <>
      "/pallette/" <>
      id <> 
      "-" <> 
      show level <> 
      mode <>
      ".png"      

  viewLevels :: String -> Option -> List.List Level -> H.ComponentHTML Query
  viewLevels id (Option opt) levels =
    let
      optionise lvl = 
        HH.option [ HP.value (show lvl) 
                  , HP.selected (opt.level == lvl) 
                  ] 
                  [ HH.text (show lvl) ]

      levelInput f = map (HQ.action <<< f <<< Level) <<< fromString
    in                  
      case levels of
        Level 1 : List.Nil ->
          HH.text ""

        (Level level) : List.Nil ->
          HH.text ("Level: " <> show level)

        xs ->
          HH.div []
                 [ HH.div [ HP.class_ (HH.ClassName "pallette-label") ] 
                          [ HH.label [] [ HH.text "Level: " ] ]
                 , HH.select [ HE.onValueChange (levelInput (LevelChange id))
                  ] $ toUnfoldable (map optionise levels)
                 ]

  viewModes :: String -> Option -> List.List Mode -> H.ComponentHTML Query
  viewModes id (Option opt) modes = 
    let 
      optionise (Mode mode) = 
        HH.option [ HP.value mode.id 
                  , HP.selected (isSelected mode && not (isDisabled mode))
                  , HP.disabled (isDisabled mode)
                  ] 
                  [ HH.text mode.name ]

      modeInput f = map (HQ.action <<< f) <<< Just
    in
      case modes of
        List.Nil ->
          HH.text ""

        xs ->
          HH.div []
                 [ HH.div [ HP.class_ (HH.ClassName "pallette-label") ] 
                          [ HH.label [] [ HH.text "Mode: " ] ]
                 , HH.select [ HE.onValueChange (modeInput (ModeChange id))
                  ] $ toUnfoldable (map optionise modes)
                 ]
    where
      isSelected mode = Just mode.id == opt.mode

      isDisabled mode = List.elem mode.id opt.disabledModes ||
                        List.elem mode.id opt.lockedModes

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ItemSelected id next -> do
      state <- H.get
      let nextState = state { selector = selectItem id state.selector }
      H.put nextState
      H.raise $ PalletteSelection (currentlySelected nextState.selector)
      pure next

    LevelChange id level next -> do
      state <- H.get
      let nextState = state { selector = changeLevelSelection id level state.selector }
      H.put nextState
      H.raise $ PalletteSelection (currentlySelected nextState.selector)
      pure next

    ModeChange id mode next -> do
      state <- H.get
      let nextState = state { selector = changeModeSelection id mode state.selector }
      H.put nextState
      H.raise $ PalletteSelection (currentlySelected nextState.selector)
      pure next

    SelectedTownHallDefinition def next -> do
      oldState <- H.get
      let nextState = resetStateIfNeeded def oldState
      H.put nextState
      H.raise $ PalletteSelection (currentlySelected nextState.selector)
      pure next

  resetStateIfNeeded :: Maybe TownHallDefinition -> State -> State
  resetStateIfNeeded definition state =
    (fromMaybe emptyState <<< map (propogateOrReplace state)) $ definition
    where
      emptyState = { selector: emptySelector, townHallLevel: Nothing }

      propogateOrReplace state' def =
        let
          (TownHallDefinition d) = def
        in
          if (Just d.level == state'.townHallLevel)
          then state'
          else { selector: freshSelector def, townHallLevel: Just d.level }

