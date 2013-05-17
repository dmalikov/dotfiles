module Environment.T510 where
import Control.Lens (set)
import Data.Default (def)
import Data.Monoid ((<>))

import Biegunka

import Environment.Base
import Profiles


template :: Template
template = def
  { xmonad = def
    { terminus_font = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-*"
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

