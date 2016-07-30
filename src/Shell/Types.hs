module Shell.Types (
  ShellState (..)
) where

import           Data.Text (Text)

data ShellState = ShellState {
  shAuthToken :: Maybe String,
  shToggle :: Bool,
  shName   :: String,
  shHist   :: Maybe String
} deriving (Eq, Ord, Show)
