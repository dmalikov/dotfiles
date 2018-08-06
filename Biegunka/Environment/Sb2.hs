{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment.Sb2 where

import           Data.Default (def)

import           Control.Biegunka hiding (shell)
import           Environment.Defaults
import           Namespaces

configs :: Configs
configs = def
  { tmux = def
    { shell = "/bin/bash"
    }
  }

namespaces :: Script 'Sources ()
namespaces = sequence_
  [ namespace_git
  , namespace_shell
  , namespace_tmux
  , namespace_vifm
  , namespace_vim
  , namespace_zsh
  ]
