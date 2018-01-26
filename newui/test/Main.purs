module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.Runner (RunnerEffects)

import Model.TownHallDefinitionsSpec (townHallDefinitionsSpec)

main :: Eff (RunnerEffects ()) Unit
main = do
  townHallDefinitionsSpec
