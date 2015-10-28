{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment.Qumsrc where

import           Data.Default         (def)

import           Control.Biegunka     hiding (shell)
import           Environment.Defaults
import           Namespaces

configs :: Configs
configs = def
  { tmux = def
    { shell = "/bin/zsh"
    }
  }

namespaces :: Script 'Sources ()
namespaces = sequence_
  [ namespace_git
  , namespace_emacs
  , namespace_haskell
  , namespace_ncmpcpp
  , namespace_mpd
  , namespace_shell
  , namespace_tmux
  , namespace_transmission
  , namespace_vifm
  , namespace_vim
  , namespace_zsh
  ]
