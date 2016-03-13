{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment.Lnd where

import           Data.Default         (def)

import           Control.Biegunka     hiding (shell)
import           Environment.Defaults
import           Namespaces

configs :: Configs
configs = def
  { tmux = def
    { shell = "/run/current-system/sw/bin/zsh"
    }
  }

namespaces :: Script 'Sources ()
namespaces = sequence_
  [ namespace_git
  , namespace_haskell
  , namespace_shell
  , namespace_tmux
  , namespace_vim
  , namespace_zsh
  ]
