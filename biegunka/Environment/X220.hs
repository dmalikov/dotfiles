module Environment.X220 where

import Data.Default (def)

import Environment.Base
import Profiles


settings :: Template
settings = def
  { git = def
    { set_user = True
    , user_name = "Dmitry Malikov"
    , user_email = "malikov.d.y"
    }
  , pentadactyl = def
    { font_size = "8pt"
    }
  , xresource_shiva = def
    { shiva_bold_font = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-iso10646-1"
    , shiva_font = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-iso10646-1"
    }
  , xresource_scratchpad = def
    { scratchpad_bold_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    , scratchpad_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    }
  , xmonad = def
    { terminus_font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-*"
    }
  , x = def
    { user = "m"
    , xft_dpi = "120"
    }
  }

profiles = sequence_
  [ profile_ackrc
  , profile_ghc
  , profile_git
  , profile_gtk
  , profile_misc
  , profile_mpd
  , profile_mplayer
  , profile_pentadactyl
  , profile_ruby
  , profile_shell
  , profile_tmux
  , profile_uzbl
  , profile_vifm
  , profile_vim
  , profile_x
  , profile_xmonad
  ]

