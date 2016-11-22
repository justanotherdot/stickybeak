module OS
  ( OS (..)
  , detectOS
  ) where

import           System.Info

data OS = Linux
  | Mac
  | Windows
  | Unsupported
  deriving (Show, Eq)

detectOS :: OS
detectOS = case os of
            "linux"  -> Linux
            "darwin" -> Mac
            -- "windows" -> Windows -- Windows not yet supported.
            _        -> Unsupported
