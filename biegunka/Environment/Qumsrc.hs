{-# LANGUAGE OverloadedStrings #-}
module Environment.Qumsrc where

import Data.Default (def)

import Environment.Defaults
import Profiles

configs :: Configs
configs = def
  { git = def
    { credentials = Just GitCredentials
      { user_name = "Dmitry Malikov"
      , user_email = "malikov.d.y@gmail.com"
      }
    }
  , tmux = def
    { shell = "/bin/zsh"
    }
  }

profiles = sequence_
  [ profile_git
  , profile_haskell
  , profile_ncmpcpp
  , profile_shell
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_zsh
  ]
