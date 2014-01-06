{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Base where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { git :: Git
  , pentadactyl :: Pentadactyl
  , urxvt :: Urxvt
  , x :: X
  , xmonad :: Xmonad
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { git = def
    , pentadactyl = def
    , urxvt = def
    , x = def
    , xmonad = def
    }

data Git = Git
  { set_user :: Bool
  , user_email :: String
  , user_name :: String
  } deriving (Data, Typeable)

data Pentadactyl = Pentadactyl
  { font_size :: Int
  } deriving (Data, Typeable)

data Urxvt = Urxvt
  { font :: String
  , boldFont :: String
  } deriving (Data, Typeable)

data X = X
  { xft_dpi :: Int
  } deriving (Data, Typeable)

data Xmonad = Xmonad
  { terminus_font :: String
  } deriving (Data, Typeable)

instance Default Git where
  def = Git
    { set_user = False
    , user_name = def
    , user_email = def
    }

instance Default Pentadactyl where
  def = Pentadactyl { font_size = def }

instance Default X where
  def = X { xft_dpi = def }

instance Default Urxvt where
  def = Urxvt
    { font = def
    , boldFont = def
    }

instance Default Xmonad where
  def = Xmonad
    { terminus_font = def
    }
