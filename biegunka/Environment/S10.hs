module Environment.S10 where

import           Data.Default     (def)

import           Environment.Base
import           Profiles


settings :: Template
settings = def

profiles = sequence_
  [ profile_git
  , profile_tmux
  ]

