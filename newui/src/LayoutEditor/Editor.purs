module LayoutEditor.Editor where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..))
import Data.List as List

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX

import Model.CoreTypes (PlacedItem)
import Model.TownHallDefinitions (TownHallDefinition)
import Model.ItemsSelector (ItemSelector, emptySelector, freshSelector, consumeItems, currentlySelected)

import LayoutEditor.Overview as Overview
import LayoutEditor.Toolbar as Toolbar
import LayoutEditor.Pallette as Pallette
import LayoutEditor.Grid as Grid

data Query a = OverviewUpdated Overview.Message a
             | ToolbarAction Toolbar.Message a
             | PalletteUpdated Pallette.Message a
             | GridUpdated Grid.Message a

type State =
  { townHallDefinition :: Maybe TownHallDefinition
  , layoutName :: Maybe String
  , wallDrawingMode :: Boolean
  , selector :: ItemSelector
  }

type ChildQuery = Coproduct4 Overview.Query Toolbar.Query Pallette.Query Grid.Query

type ChildSlot = Either4 Unit Unit Unit Unit

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
    , selector: emptySelector
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
  render state = 
    HH.div [] 
           [ HH.slot' CP.cp1 unit Overview.component unit (HE.input OverviewUpdated)
           , HH.slot' CP.cp2 unit Toolbar.component unit (HE.input ToolbarAction)
           , HH.slot' CP.cp3 unit Pallette.component state.selector (HE.input PalletteUpdated)
           , HH.slot' CP.cp4 unit Grid.component (currentlySelected state.selector) (HE.input GridUpdated)
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

    PalletteUpdated (Pallette.SelectorUpdate selector) next -> do
      H.modify (applySelectorUpdate selector)
      pure next

    GridUpdated (Grid.PlacedItemsChange items) next -> do
      H.modify (applyPlacedItemsUpdate items)
      pure next

  applyDefinitionUpdated :: Either String TownHallDefinition -> State -> State
  applyDefinitionUpdated definition state =
    case definition of
      Left _ -> 
        state      -- error loading definiton
      Right def ->
        state { townHallDefinition = Just def
              , selector = freshSelector def
              }

  applyLayoutNameUpdated :: Maybe String -> State -> State
  applyLayoutNameUpdated name state =
    state { layoutName = name }

  applySelectorUpdate :: ItemSelector -> State -> State
  applySelectorUpdate selector state =
    state { selector = selector }

  applyPlacedItemsUpdate :: List.List PlacedItem -> State -> State
  applyPlacedItemsUpdate items state =
    state { selector = consumeItems items state.selector }
