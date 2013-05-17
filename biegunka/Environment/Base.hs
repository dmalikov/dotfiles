{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Base where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { pentadactyl :: Pentadactyl
  , xresource_scratchpad :: XresourceScratchpad
  , xresource_shiva :: XresourceShiva
  , xmonad :: Xmonad
  , x :: X
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { pentadactyl = def
    , xresource_scratchpad = def
    , xresource_shiva = def
    , xmonad = def
    , x = def
    }

data Pentadactyl = Pentadactyl
  { font_size :: String
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
  { xft_dpi :: String
  } deriving (Data, Typeable)

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
    { xft_dpi = def
    }
