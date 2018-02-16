module LayoutEditor.Grid where

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

import Model.CoreTypes (Level(..), PlacedItem)
import Model.Layout (Layout, makeLayout)
--import Model.TownHallDefinitions (TownHallDefinition(..), AllowedBuilding, Mode(..))
--import Model.ItemsSelector (ItemSelector, changeLevelSelection, changeModeSelection, emptySelector, freshSelector, selectItem, selectableBuildings, BuildingInfo, buildingInfo, currentlySelected)

type State = 
  { layout :: Layout
  , nextItem :: Maybe PlacedItem
  }

data Query a = SelectedItem (Maybe PlacedItem) a

type Input = Maybe PlacedItem

data Message = PlacedItemsChange (List.List PlacedItem)

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input SelectedItem
    }
  where

  initialState :: State
  initialState = 
    { layout: makeLayout
    , nextItem: Nothing  
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.ClassName "map") ]
           []

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    SelectedItem item next -> do
      state <- H.get
      let nextState = state { nextItem = item }
      H.put nextState
      pure next
