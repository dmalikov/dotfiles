{-# LANGUAGE DataKinds, DeriveDataTypeable #-}
module Environment.Defaults where

import           Control.Lens                               (set)
import           Control.Monad                              (void)
import           Data.Default

import           Control.Biegunka                           hiding (shell)
import           Control.Biegunka.Templates.HStringTemplate

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Runner a = (Settings () -> Settings ()) -> Script Sources () -> IO a

class Environmentable a where
  configs :: a -> Configs
  profiles :: a -> Script Sources ()

  rollout :: a -> Runner a -> IO ()
  rollout x r = void $ r (set root "~" . set templates (hStringTemplate (configs x))) (profiles x)

data Environment = W540 | S10 | Qumsrc
  deriving (Data, Typeable)

data Configs = Configs
  { git         :: Git
  , pentadactyl :: Pentadactyl
  , tmux        :: Tmux
  , urxvt       :: Urxvt
  , x           :: X
  , xmonad      :: Xmonad
  } deriving (Data, Typeable)

instance Default Configs where
  def = Configs
    { git = def
    , pentadactyl = def
    , tmux = def
    , urxvt = def
    , x = def
    , xmonad = def
    }

data Git = Git
  { credentials :: Maybe GitCredentials
  } deriving (Data, Typeable)

data GitCredentials = GitCredentials
  { user_email :: String
  , user_name  :: String
  } deriving (Data, Typeable)

data Pentadactyl = Pentadactyl
  { font_size :: Int
  } deriving (Data, Typeable)

data Tmux = Tmux
  { shell :: String
  } deriving (Data, Typeable)

data Urxvt = Urxvt
  { font :: String
  } deriving (Data, Typeable)

data X = X
  { xft_dpi :: Int
  } deriving (Data, Typeable)

data Xmonad = Xmonad
  { terminus_font :: String
  } deriving (Data, Typeable)

instance Default Git where
  def = Git { credentials = Nothing }

instance Default Pentadactyl where
  def = Pentadactyl { font_size = def }

instance Default Tmux where
  def = Tmux { shell = def }

instance Default X where
  def = X { xft_dpi = def }

instance Default Urxvt where
  def = Urxvt { font = def }

instance Default Xmonad where
  def = Xmonad { terminus_font = def }
