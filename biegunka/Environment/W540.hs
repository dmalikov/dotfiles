{-# LANGUAGE OverloadedStrings #-}
module Environment.W540 where

import           Data.Default         (def)

import           Environment.Defaults
import           Profiles


configs :: Configs
configs = def
  { git = def
    { set_user = True
    , user_name = "Dmitry Malikov"
    , user_email = "malikov.d.y@gmail.com"
    }
  , urxvt = def
    { font = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1"
    }
  , x = def
    { xft_dpi = 125
    }
  , tmux = def
    { shell = "/run/current-system/sw/bin/zsh"
    }
  }


profiles = sequence_
  [ profile_git
  , profile_haskell
  , profile_i3
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_x
  , profile_zsh
  ]

