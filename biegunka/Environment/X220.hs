module Environment.X220 where

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
  , pentadactyl = def
    { font_size = 8
    }
  , tmux = def
    { shell = "/bin/zsh"
    }
  , urxvt = def
    { font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-1"
    }
  , x = def
    { xft_dpi = 125
    }
  , xmonad = def
    { terminus_font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-1"
    }
  }

profiles = sequence_
  [ profile_ackrc
  , profile_bash
  , profile_git
  , profile_gtk
  , profile_haskell
  , profile_mpd
  , profile_mplayer
  , profile_mpv
  , profile_pentadactyl
  , profile_ruby
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_x
  , profile_xmonad
  , profile_zathura
  , profile_zsh
  ]

