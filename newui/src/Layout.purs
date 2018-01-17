module Layout where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Event as HE
import Halogen.HTML.Properties as HP

newtype Level = Level Int

type State =
  { townHallLevels :: List Level 
  , townHallLevel :: Maybe Level
  , layoutName :: Maybe String
  }

data Query a = LevelChange Level a
             | NameChange String a

type Input = Unit

data Message = Foo

layoutInformation :: forall m. H.Component HH.HTML Query Input Message m
layoutInformation =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 
    { townHallLevels = 11 :: 10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: Nil
    , townHallLevel = Nothing
    , layoutName = Nothing 
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [] [ HH.text "Testing" ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Tbc -> do


