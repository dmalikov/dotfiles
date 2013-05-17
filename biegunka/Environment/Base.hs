{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Base where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { xmonad :: Xmonad
  , x :: X
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { xmonad = def

    }

data Xmonad = Xmonad
  { terminus_font :: String
  } deriving (Data, Typeable)

data X = X
  { xft_dpi :: String
  } deriving (Data, Typeable)

instance Default Xmonad where
  def = Xmonad
    { terminus_font = def
    }

instance Default X where
  def = X
    { xft_dpi = def
    }
