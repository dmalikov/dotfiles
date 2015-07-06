{-# LANGUAGE OverloadedStrings #-}
module Environment.S10 where

import           Data.Default         (def)

import           Environment.Defaults
import           Namespaces

configs :: Configs
configs = def
  { urxvt = def
    { font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-1"
    }
  , x = def
    { xft_dpi = 125
    }
  , tmux = def
    { shell = "~/.nix-profile/bin/zsh"
    }
  }

namespaces = sequence_
  [ namespace_git
  , namespace_haskell
  , namespace_ncmpcpp
  , namespace_shell
  , namespace_tmux
  , namespace_vifm
  , namespace_vim
  , namespace_x
  , namespace_xmonad
  , namespace_zsh
  ]
