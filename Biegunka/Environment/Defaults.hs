{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric #-}
module Environment.Defaults where

import           Data.Data
import           Data.Default

import           Control.Biegunka                           hiding (shell)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Environment = W540 | S10 | Qumsrc
  deriving (Bounded, Enum, Generic)

instance Environments Environment

data Configs = Configs
  { tmux  :: Tmux
  , urxvt :: Urxvt
  , x     :: X
  } deriving (Data, Generic)

instance Default Configs where
  def = Configs
    { tmux = def
    , urxvt = def
    , x = def
    }

data Tmux = Tmux
  { shell :: String
  } deriving (Data, Generic)

data Urxvt = Urxvt
  { font :: String
  } deriving (Data, Generic)

data X = X
  { xft_dpi :: Int
  } deriving (Data, Generic)

instance Default Tmux where
  def = Tmux { shell = def }

instance Default X where
  def = X { xft_dpi = def }

instance Default Urxvt where
  def = Urxvt { font = def }
