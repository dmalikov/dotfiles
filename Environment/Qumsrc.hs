{-# LANGUAGE OverloadedStrings #-}
module Environment.Qumsrc where

import           Data.Default         (def)

import           Environment.Defaults
import           Namespaces

configs :: Configs
configs = def
  { tmux = def
    { shell = "/bin/zsh"
    }
  }

namespaces = sequence_
  [ namespace_git
  , namespace_emacs
  , namespace_haskell
  , namespace_ncmpcpp
  , namespace_mpd
  , namespace_shell
  , namespace_tmux
  , namespace_vifm
  , namespace_vim
  , namespace_zsh
  ]
