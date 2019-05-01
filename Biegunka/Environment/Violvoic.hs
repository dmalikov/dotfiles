{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment.Violvoic where

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
  , namespace_haskell
  , namespace_mpd
  , namespace_nix
  , namespace_ncmpcpp
  , namespace_shell
  , namespace_tmux
  , namespace_transmission
  , namespace_vim
  , namespace_zsh
  ]
