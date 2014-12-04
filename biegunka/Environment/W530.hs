{-# LANGUAGE OverloadedStrings #-}
module Environment.W530 where

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
  , tmux = def
    { shell = "/run/current-system/sw/bin/zsh"
    }
  }


profiles = sequence_
  [ profile_git
  , profile_haskell
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_zsh
  ]

