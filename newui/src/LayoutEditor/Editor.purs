module LayoutEditor.Editor where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Either (Either, hush)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX

import Model.CoreTypes (PlacedItem)
import Model.TownHallDefinitions (TownHallDefinition)
import LayoutEditor.Overview as Overview
import LayoutEditor.Toolbar as Toolbar
import LayoutEditor.Pallette as Pallette

data Query a = OverviewUpdated Overview.Message a
             | ToolbarAction Toolbar.Message a
             | PalletteUpdated Pallette.Message a

type State =
  { townHallDefinition :: Maybe TownHallDefinition
  , layoutName :: Maybe String
  , wallDrawingMode :: Boolean
  , selectedItem :: Maybe PlacedItem 
  }

type ChildQuery = Coproduct3 Overview.Query Toolbar.Query Pallette.Query

type ChildSlot = Either3 Unit Unit Unit

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { townHallDefinition: Nothing
    , layoutName: Nothing
    , wallDrawingMode: true
    , selectedItem: Nothing
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
  render state = 
    HH.div [] 
           [ HH.slot' CP.cp1 unit Overview.component unit (HE.input OverviewUpdated)
           , HH.slot' CP.cp2 unit Toolbar.component unit (HE.input ToolbarAction)
           , HH.slot' CP.cp3 unit Pallette.component state.townHallDefinition (HE.input PalletteUpdated)
           ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    OverviewUpdated (Overview.DefinitionChange definition) next -> do
      H.modify (applyDefinitionUpdated definition)
      pure next

    OverviewUpdated (Overview.LayoutNameChange name) next -> do
      H.modify (applyLayoutNameUpdated name)
      pure next

    ToolbarAction _ next -> do
      -- TODO
      pure next

    PalletteUpdated (Pallette.PalletteSelection item) next -> do
      H.modify (applyPalletteSelection item)
      pure next

  applyDefinitionUpdated :: Either String TownHallDefinition -> State -> State
  applyDefinitionUpdated definition state =
    state { townHallDefinition = hush definition }

  applyLayoutNameUpdated :: Maybe String -> State -> State
  applyLayoutNameUpdated name state =
    state { layoutName = name }

  applyPalletteSelection :: Maybe PlacedItem -> State -> State
  applyPalletteSelection item state =
    state { selectedItem = item }
