{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module Profiles where
import           Control.Lens
import           Data.Default                (def)

import           Control.Biegunka
import           Control.Biegunka.Source.Git

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:dmalikov/dotfiles" "git/dotfiles" $ def & actions .~ as

profile_vim :: Script Sources ()
profile_vim = do
  profile "vim/rc" $ do
    git_ "git@github.com:Shougo/neobundle.vim" ".vim/bundle/neobundle.vim"
    git "git@github.com:tpope/vim-pathogen" ".vim/bundle/vim-pathogen" $
      copy "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  profile "vim/syntax" $ do
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
    dotfiles $ copy "configs/vim/syntax/nix.vim" ".vim/after/syntax/nix.vim"
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
  dotfiles $
    copy "configs/mpd/conf" ".mpdconf"

profile_ncmpcpp :: Script Sources ()
profile_ncmpcpp = profile "ncmpcpp" $
  dotfiles $
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

profile_bash :: Script Sources ()
profile_bash = profile "bash" $
  dotfiles $
    copy "configs/bash/bashrc" ".bashrc"

profile_zsh :: Script Sources ()
profile_zsh = profile "zsh" $ do
  git_ "git@github.com:zsh-users/zsh-completions" "git/zsh-completions"
  git_ "git@github.com:zsh-users/zsh-syntax-highlighting" "git/zsh-syntax-highlighting"
  dotfiles $ do
    copy "configs/zsh/zshrc" ".zshrc"
    copy "configs/zsh/zshenv" ".zshenv"
    copy "configs/zsh/zprofile" ".zprofile"

profile_java :: Script Sources ()
profile_java = profile "idea" $
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

profile_tmux :: Script Sources ()
profile_tmux = profile "tmux" $ do
  git_ "git@github.com:richo/battery" "git/tmux-battery"
  dotfiles $ do
    substitute "configs/tmux/conf.template" ".tmux.conf"
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
    copy "configs/vifm/colors/hybrid" ".vifm/colors/hybrid"
    copy "configs/vifm/vifmrc" ".vifm/vifmrc"

profile_zathura :: Script Sources ()
profile_zathura = profile "zathura" $
  dotfiles $
    copy "configs/zathura/zathurarc" ".config/zathura/zathurarc"

profile_nixpkgs :: Script Sources ()
profile_nixpkgs = profile "nixpkgs" $ do
  git' "git@github.com:biegunka/biegunka" "git/biegunka" (def & branch .~ "develop")
  git_ "git@github.com:nixos/cabal2nix" "git/cabal2nix"
  git_ "git@github.com:supki/scrobblers" "git/scrobblers"
  git_ "git@github.com:supki/liblastfm" "git/liblastfm"
  dotfiles $ do
    copy "nixpkgs/config.nix" ".nixpkgs/config.nix"
    copy "nixpkgs/cabal2nix/default.nix" ".nixpkgs/cabal2nix/default.nix"
    copy "nixpkgs/hstorrent/default.nix" ".nixpkgs/hstorrent/default.nix"
    copy "nixpkgs/hspecContrib/default.nix" ".nixpkgs/hspecContrib/default.nix"
