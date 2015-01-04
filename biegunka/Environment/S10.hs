{-# LANGUAGE OverloadedStrings #-}
module Environment.S10 where

import           Data.Default     (def)

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
    { shell = "~/.nix-profile/bin/zsh"
    }
  }


profiles = sequence_
  [ profile_git
  , profile_haskell
  , profile_ncmpcpp
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_x
  , profile_xmonad
  , profile_zsh
  ]

