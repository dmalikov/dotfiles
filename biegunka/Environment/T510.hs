module Environment.T510 (settings, profiles) where

import Data.Default (def)

import Environment.Base
import Profiles

settings :: Template
settings = def
  { pentadactyl = def
    { font_size = "10pt"
    }
  , xresource_shiva = def
    { shiva_bold_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    , shiva_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    }
  , xresource_scratchpad = def
    { scratchpad_bold_font = "-*-terminus-medium-r-*-*-18-*-*-*-*-*-iso10646-1"
    , scratchpad_font = "-*-terminus-medium-r-*-*-18-*-*-*-*-*-iso10646-1"
    }
  , xmonad = def
    { terminus_font = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-*"
    }
  , x = def
    { user = "dmalikov"
    , xft_dpi = "144"
    }
  }

profiles = sequence_
  [ profile_vim
  , profile_xmonad
  , profile_git
  , profile_ruby
  , profile_x
  , profile_ghc
  , profile_mpd
  , profile_mplayer
  , profile_pentadactyl
  , profile_ackrc
  , profile_shell
  , profile_java
  , profile_gtk
  , profile_tmux
  , profile_uzbl
  , profile_vifm
  , profile_misc
  ]
