{-# LANGUAGE OverloadedStrings #-}
module Environment.Qumsrc where

import           Data.Default         (def)

import           Environment.Defaults
import           Profiles

configs :: Configs
configs = def
  { tmux = def
    { shell = "/bin/zsh"
    }
  }

profiles = sequence_
  [ profile_git
  , profile_haskell
  , profile_ncmpcpp
  , profile_mpd
  , profile_shell
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_zsh
  ]
