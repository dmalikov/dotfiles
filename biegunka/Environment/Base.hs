{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Base where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { xresource_scratchpad :: XresourceScratchpad
  , sresource_shiva :: XresourceShiva
  , xmonad :: Xmonad
  , x :: X
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { xresource_scratchpad = def
    , xresource_shiva = def
    , xmonad = def
    , X = def
    }

data XresourceScratchpad = XresourceScratchpad
  { scratchpad_bold_font :: String
  , scratchpad_font :: String
  } deriving (Data, Typeable)

data XresourceShiva = XresourceScratchpad
  { shiva_bold_font :: String
  , shiva_font :: String
  } deriving (Data, Typeable)

data Xmonad = Xmonad
  { terminus_font :: String
  } deriving (Data, Typeable)

data X = X
  { xft_dpi :: String
  } deriving (Data, Typeable)

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
    { xft_dpi = def
    }
