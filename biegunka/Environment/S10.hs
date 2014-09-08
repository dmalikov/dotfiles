{-# LANGUAGE OverloadedStrings #-}
module Environment.S10 where

import           Data.Default     (def)

import           Environment.Base
import           Profiles


settings :: Template
settings = def
  { git = def
    { set_user = True
    , user_name = "Dmitry Malikov"
    , user_email = "malikov.d.y@gmail.com"
    }
  , tmux = def
    { shell = "${HOME}/.nix-profile/bin/zsh"
    }
  }


profiles = sequence_
  [ profile_git
  , profile_tmux
  , profile_zsh
  , profile_vim
  , profile_haskell
  , profile_nixpkgs
  ]
