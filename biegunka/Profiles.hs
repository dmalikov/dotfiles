{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module Profiles where
import           Control.Lens
import           Data.Default                (def)

import           Control.Biegunka
import           Control.Biegunka.Source.Git

dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:dmalikov/dotfiles" "projects/dmalikov/dotfiles" $ def & actions .~ as

profile_vim :: Script Sources ()
profile_vim = do
  profile "vim/rc" $ do
    git_ "Shougo/neobundle.vim" ".vim/bundle/neobundle.vim"
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  profile "vim/syntax" $
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
  profile "vim/colorschemes" $
    dotfiles $ copy "configs/vim/colors/neverland-darker.vim" ".vim/colors/neverland-darker.vim"
  profile "vim/plugins" $
    dotfiles $ copy "configs/vim/MyTabularMaps.vim" ".vim/bundle/tabular/after/plugin/MyTabularMaps.vim"

profile_xmonad :: Script Sources ()
profile_xmonad =
  profile "xmonad/xmonad.hs" $
    dotfiles $
      substitute "configs/xmonad/xmonad.hs.template" ".xmonad/xmonad.hs"

profile_git :: Script Sources ()
profile_git = profile "git" $
  dotfiles $ do
    substitute "configs/git/config.template" ".gitconfig"
    copy "configs/git/ignore" ".gitignore"
    copy "configs/tig/tigrc" ".tigrc"

profile_ruby :: Script Sources ()
profile_ruby = profile "ruby" $
  dotfiles $ do
    copy "configs/ruby/irbrc" ".irbrc"
    copy "configs/ruby/rvmrc" ".rvmrc"

profile_x :: Script Sources ()
profile_x = profile "X" $
  dotfiles $ do
    copy "configs/X/XCompose" ".XCompose"
    substitute "configs/X/Xresources.template" ".Xresources"
    copy "configs/X/colors/shiva" ".urxvt/colors/shiva"
    copy "configs/X/colors/hybrid" ".urxvt/colors/hybrid"
    copy "configs/X/inputrc" ".inputrc"
    copy "configs/X/startup" ".startup"
    copy "configs/X/xinitrc" ".xinitrc"

profile_haskell :: Script Sources ()
profile_haskell = profile "haskell" $
  dotfiles $ do
    copy "configs/ghc/ghci" ".ghci"
    copy "configs/ghc/stylish-haskell.yaml" ".stylish-haskell.yaml"
    copy "configs/ghc/haskeline" ".haskeline"
    copy "configs/guard/guard.rb" ".guard.rb"

profile_irssi :: Script Sources ()
profile_irssi = profile "irssi" $
  dotfiles $ do
    copy "configs/irssi/bleeding.theme" ".irssi/bleeding.theme"
    copy "configs/irssi/config" ".irssi/config"

profile_mpd :: Script Sources ()
profile_mpd = profile "mpd" $
  dotfiles $ do
    copy "configs/mpd/mpdconf" ".mpdconf"
    copy "configs/mpd/ncmpcpp/config" ".ncmpcpp/config"

profile_mplayer :: Script Sources ()
profile_mplayer = profile "mplayer" $
  dotfiles $ do
    copy "configs/mplayer/config" ".mplayer/config"
    copy "configs/mplayer/input.conf" ".mplayer/input.conf"

profile_mpv :: Script Sources ()
profile_mpv = profile "mpv" $
  dotfiles $
    copy "configs/mpv/input.conf" ".mpv/input.conf"

profile_pentadactyl :: Script Sources ()
profile_pentadactyl = profile "pentadactyl" $
  dotfiles $ do
    substitute "configs/pentadactyl/colors/pemees.penta.template" ".pentadactyl/colors/pemees.penta"
    copy "configs/pentadactyl/pentadactylrc" ".pentadactylrc"
    copy "configs/pentadactyl/plugins/buftabs.js" ".pentadactyl/plugins/buftabs.js"

profile_screen :: Script Sources ()
profile_screen = profile "screen" $
  dotfiles $
    copy "configs/screen/screenrc" ".screenrc"

profile_ackrc :: Script Sources ()
profile_ackrc = profile "ack" $
  dotfiles $
    copy "configs/ack/ackrc" ".ackrc"

profile_apvlv :: Script Sources ()
profile_apvlv = profile "apvlv" $
  dotfiles $
    copy "configs/apvlv/apvlvrc" ".apvlvrc"

profile_shell :: Script Sources ()
profile_shell = do
  profile "bash" $
    dotfiles $
      copy "configs/bash/bashrc" ".bashrc"
  profile "zsh" $ do
    git_ "git@github.com:zsh-users/zsh-completions" "projects/misc/"
    dotfiles $ do
      copy "configs/zsh/zshrc" ".zshrc"
      copy "configs/zsh/zshenv" ".zshenv"
      copy "configs/zsh/zprofile" ".zprofile"
      copy "configs/zsh/tmux" ".zsh/tmux"

profile_java :: Script Sources ()
profile_java =
  profile "idea" $
    dotfiles $
      copy "configs/idea/config/colors/bleedie.xml" ".IdeaIC12/config/colors/bleedie.xml"

profile_conky :: Script Sources ()
profile_conky = profile "conky" $
  dotfiles $
    copy "configs/conky/conkyrc" ".conkyrc"

profile_gtk :: Script Sources ()
profile_gtk = profile "gtk" $
  dotfiles $
    copy "configs/gtk/gtkrc-2.0.mine" ".gtkrc-2.0.mine"

profile_mocp :: Script Sources ()
profile_mocp = profile "mocp" $
  dotfiles $
    copy "configs/moc/config" ".moc/config"

profile_tmux :: Script Sources ()
profile_tmux = profile "tmux" $
  dotfiles $ do
    copy "configs/tmux/conf" ".tmux.conf"
    copy "configs/tmux/keybindings" ".tmux.keybindings"

profile_uzbl :: Script Sources ()
profile_uzbl = profile "uzbl" $
  dotfiles $
    copy "configs/uzbl/config" ".config/uzbl/config"

profile_dwb :: Script Sources ()
profile_dwb = profile "dwb" $
  dotfiles $ do
    copy "configs/dwb/settings" ".config/dwb/settings"
    copy "configs/dwb/keys" ".config/dwb/keys"

profile_vifm :: Script Sources ()
profile_vifm = profile "vifm" $
  dotfiles $ do
    copy "configs/vifm/colors/neverland" ".vifm/colors/neverland"
    copy "configs/vifm/vifmrc" ".vifm/vifmrc"

profile_zathura :: Script Sources ()
profile_zathura = profile "zathura" $
  dotfiles $
    copy "configs/zathura/zathurarc" ".config/zathura/zathurarc"
