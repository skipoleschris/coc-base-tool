module LayoutEditor.Editor where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Model.CoreTypes (Level)
import LayoutEditor.Overview as Overview
import LayoutEditor.Toolbar as Toolbar

data Query a = OverviewUpdated Overview.Message a
             | ToolbarAction Toolbar.Message a

type State =
  { townHallLevel :: Maybe Level
  , layoutName :: Maybe String
  , wallDrawingMode :: Boolean 
  }

type ChildQuery = Coproduct2 Overview.Query Toolbar.Query

type ChildSlot = Either2 Unit Unit

component :: forall m. Applicative m => H.Component HH.HTML Query Unit Void m
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
    { townHallLevel: Nothing
    , layoutName: Nothing
    , wallDrawingMode: true
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = 
    HH.div [] 
           [ HH.slot' CP.cp1 unit Overview.component unit (HE.input OverviewUpdated)
           , HH.slot' CP.cp2 unit Toolbar.component unit (HE.input ToolbarAction)
           ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    OverviewUpdated (Overview.LayoutInformation level name) next -> do
      H.modify (applyLayoutUpdated level name)
      pure next

    ToolbarAction _ next -> do
      -- TODO
      pure next

  applyLayoutUpdated :: Maybe Level -> Maybe String -> State -> State
  applyLayoutUpdated level name state =
    state { townHallLevel = level, layoutName = name }
