module LayoutEditor.Editor where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Either (Either, hush)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX

import Model.TownHallDefinitions (TownHallDefinition)
import LayoutEditor.Overview as Overview
import LayoutEditor.Toolbar as Toolbar

data Query a = OverviewUpdated Overview.Message a
             | ToolbarAction Toolbar.Message a

type State =
  { townHallDefinition :: Maybe TownHallDefinition
  , layoutName :: Maybe String
  , wallDrawingMode :: Boolean 
  }

type ChildQuery = Coproduct2 Overview.Query Toolbar.Query

type ChildSlot = Either2 Unit Unit

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
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
  render state = 
    HH.div [] 
           [ HH.slot' CP.cp1 unit Overview.component unit (HE.input OverviewUpdated)
           , HH.slot' CP.cp2 unit Toolbar.component unit (HE.input ToolbarAction)
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

  applyDefinitionUpdated :: Either String TownHallDefinition -> State -> State
  applyDefinitionUpdated definition state =
    state { townHallDefinition = hush definition }

  applyLayoutNameUpdated :: Maybe String -> State -> State
  applyLayoutNameUpdated name state =
    state { layoutName = name }
