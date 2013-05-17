module Environment.X220 where

import Control.Lens (set)
import Data.Default (def)
import Data.Monoid ((<>))

import Biegunka

import Environment.Base
import Profiles


template :: Template
template = def
  { pentadactyl = def
    { font_size = "8pt"
    }
  , xresource_shiva = def
    { shiva_bold_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    , shiva_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    }
  , xresource_scratchpad = def
    { scratchpad_bold_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    , scratchpad_font = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    }
  , xmonad = def
    { terminus_font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-*"
    }
  , x = def
    { xft_dpi = "120"
    }
  }

profiles = sequence_
  [ profile_vim
  , profile_xmonad
  , profile_git
  , profile_ruby
  , profile_x
  , profile_ghc
  , profile_irssi
  , profile_mpd
  , profile_mplayer
  , profile_pentadactyl
  , profile_screen
  , profile_ackrc
  , profile_apvlv
  , profile_shell
  , profile_java
  , profile_conky
  , profile_gtk
  , profile_mocp
  , profile_tmux
  , profile_uzbl
  , profile_vifm
  , profile_misc
  ]

install :: IO ()
install = biegunka (set root "~") profiles $ pretend <> pause <> execute (set templates (Templates template))
