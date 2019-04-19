{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Environment.Defaults where

import           Data.Data
import           Data.Default

import           Control.Biegunka hiding (shell)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Environment = Lnd | Qumsrc | S10 | Sb2 | Violvoic | W540
  deriving (Generic)

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
