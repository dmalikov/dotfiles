{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Base where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { git :: Git
  , pentadactyl :: Pentadactyl
  , xresource_scratchpad :: XresourceScratchpad
  , xresource_shiva :: XresourceShiva
  , xmonad :: Xmonad
  , x :: X
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { git = def
    , pentadactyl = def
    , xresource_scratchpad = def
    , xresource_shiva = def
    , xmonad = def
    , x = def
    }

data Git = Git
  { set_user :: Bool
  , user_email :: String
  , user_name :: String
  } deriving (Data, Typeable)

data Pentadactyl = Pentadactyl
  { font_size :: Int
  } deriving (Data, Typeable)

data XresourceScratchpad = XresourceScratchpad
  { scratchpad_bold_font :: String
  , scratchpad_font :: String
  } deriving (Data, Typeable)

data XresourceShiva = XresourceShiva
  { shiva_bold_font :: String
  , shiva_font :: String
  } deriving (Data, Typeable)

data Xmonad = Xmonad
  { terminus_font :: String
  } deriving (Data, Typeable)

data X = X
  { user :: String
  , xft_dpi :: Int
  } deriving (Data, Typeable)

instance Default Git where
  def = Git
    { set_user = False
    , user_name = def
    , user_email = def
    }

instance Default Pentadactyl where
  def = Pentadactyl
    { font_size = def
    }

instance Default XresourceShiva where
  def = XresourceShiva
    { shiva_bold_font = def
    , shiva_font = def
    }

instance Default XresourceScratchpad where
  def = XresourceScratchpad
    { scratchpad_bold_font = def
    , scratchpad_font = def
    }

instance Default Xmonad where
  def = Xmonad
    { terminus_font = def
    }

instance Default X where
  def = X
    { user = def
    , xft_dpi = def
    }
