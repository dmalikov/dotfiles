{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment.W540 where

import           Data.Default         (def)

import           Control.Biegunka     hiding (shell)
import           Environment.Defaults
import           Namespaces


configs :: Configs
configs = def
  { urxvt = def
    { font = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1"
    }
  , x = def
    { xft_dpi = 125
    }
  , tmux = def
    { shell = "/run/current-system/sw/bin/zsh"
    }
  }

namespaces :: Script 'Sources ()
namespaces = sequence_
  [ namespace_git
  , namespace_haskell
  , namespace_i3
  , namespace_shell
  , namespace_tmux
  , namespace_vifm
  , namespace_vim
  , namespace_x
  , namespace_zsh
  ]
