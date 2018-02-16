module Model.Layout ( Layout
                    , makeLayout ) where

-- Types

type Layout = {
  placeholder :: String
}


-- Construction functions

makeLayout :: Layout
makeLayout = {
  placeholder: "TODO"
}
