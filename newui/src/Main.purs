module Main where

import Prelude
import Control.Coroutine as CR
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import LayoutEditor.Editor as LE

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI LE.component unit body

  io.subscribe $ CR.consumer \_ -> do
    log $ "Layout editor event..."
    pure Nothing
