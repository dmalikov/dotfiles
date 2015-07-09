{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Namespaces where

import           Control.Biegunka
import           Control.Biegunka.Source.Git

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

dotfiles :: Script 'Actions () -> Script 'Sources ()
dotfiles = git (url "https://github.com/dmalikov/dotfiles" . path "git/dotfiles")

namespace_vim :: Script 'Sources ()
namespace_vim = do
  namespace "vim/rc" $ do
    git (url "https://github.com/Shougo/neobundle.vim" . path ".vim/bundle/neobundle.vim") pass
    git (url "https://github.com/tpope/vim-pathogen" . path ".vim/bundle/vim-pathogen") $
      copy "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  namespace "vim/syntax" $ do
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
    dotfiles $ copy "configs/vim/syntax/nix.vim" ".vim/after/syntax/nix.vim"
  namespace "vim/colorschemes" $
    dotfiles $ copy "configs/vim/colors/neverland-darker.vim" ".vim/colors/neverland-darker.vim"
  namespace "vim/plugins" $
    dotfiles $ copy "configs/vim/MyTabularMaps.vim" ".vim/bundle/tabular/after/plugin/MyTabularMaps.vim"

namespace_emacs :: Script 'Sources ()
namespace_emacs =
  namespace "emacs/spacemacs" $
    dotfiles $ copy "configs/emacs/spacemacs" ".spacemacs"

namespace_xmonad :: Script 'Sources ()
namespace_xmonad =
  namespace "xmonad" $
    dotfiles $
      copy "configs/xmonad/xmonad.hs" ".xmonad/xmonad.hs"

namespace_i3 :: Script 'Sources ()
namespace_i3 =
  namespace "i3" $
    dotfiles $
      copy "configs/i3/config" ".i3/config"

namespace_git :: Script 'Sources ()
namespace_git = namespace "git" $
  dotfiles $ do
    copy "configs/git/config" ".gitconfig"
    copy "configs/git/ignore" ".gitignore"
    copy "configs/tig/tigrc" ".tigrc"

namespace_ruby :: Script 'Sources ()
namespace_ruby = namespace "ruby" $
  dotfiles $ do
    copy "configs/ruby/irbrc" ".irbrc"
    copy "configs/ruby/rvmrc" ".rvmrc"

namespace_x :: Script 'Sources ()
namespace_x = namespace "X" $
  dotfiles $ do
    copy "configs/X/XCompose" ".XCompose"
    substitute "configs/X/Xresources.template" ".Xresources"
    copy "configs/X/colors/shiva" ".urxvt/colors/shiva"
    copy "configs/X/colors/hybrid" ".urxvt/colors/hybrid"

namespace_haskell :: Script 'Sources ()
namespace_haskell = namespace "haskell" $
  dotfiles $ do
    copy "configs/ghc/ghci" ".ghci"
    copy "configs/ghc/stylish-haskell.yaml" ".stylish-haskell.yaml"
    copy "configs/ghc/haskeline" ".haskeline"
    copy "configs/guard/guard.rb" ".guard.rb"

namespace_irssi :: Script 'Sources ()
namespace_irssi = namespace "irssi" $
  dotfiles $ do
    copy "configs/irssi/bleeding.theme" ".irssi/bleeding.theme"
    copy "configs/irssi/config" ".irssi/config"

namespace_mpd :: Script 'Sources ()
namespace_mpd = namespace "mpd" $
  dotfiles $
    copy "configs/mpd/conf" ".mpdconf"

namespace_ncmpcpp :: Script 'Sources ()
namespace_ncmpcpp = namespace "ncmpcpp" $
  dotfiles $ do
    copy "configs/ncmpcpp/config" ".ncmpcpp/config"
    copy "configs/ncmpcpp/bindings" ".ncmpcpp/bindings"

namespace_nix :: Script 'Sources ()
namespace_nix = namespace "nix" $
  dotfiles $ do
    copy "configs/nix/nixpkgs/config.nix" ".nixpkgs/config.nix"
    copy "configs/nix/nixpkgs/hdevtools.nix" ".nixpkgs/hdevtools.nix"

namespace_mplayer :: Script 'Sources ()
namespace_mplayer = namespace "mplayer" $
  dotfiles $ do
    copy "configs/mplayer/config" ".mplayer/config"
    copy "configs/mplayer/input.conf" ".mplayer/input.conf"

namespace_mpv :: Script 'Sources ()
namespace_mpv = namespace "mpv" $
  dotfiles $
    copy "configs/mpv/input.conf" ".mpv/input.conf"

namespace_pentadactyl :: Script 'Sources ()
namespace_pentadactyl = namespace "pentadactyl" $
  dotfiles $ do
    substitute "configs/pentadactyl/colors/pemees.penta.template" ".pentadactyl/colors/pemees.penta"
    copy "configs/pentadactyl/pentadactylrc" ".pentadactylrc"

namespace_screen :: Script 'Sources ()
namespace_screen = namespace "screen" $
  dotfiles $
    copy "configs/screen/screenrc" ".screenrc"

namespace_ackrc :: Script 'Sources ()
namespace_ackrc = namespace "ack" $
  dotfiles $
    copy "configs/ack/ackrc" ".ackrc"

namespace_apvlv :: Script 'Sources ()
namespace_apvlv = namespace "apvlv" $
  dotfiles $
    copy "configs/apvlv/apvlvrc" ".apvlvrc"

namespace_shell :: Script 'Sources ()
namespace_shell = namespace "shell" $
  dotfiles $
    copy "configs/shell/inputrc" ".inputrc"

namespace_bash :: Script 'Sources ()
namespace_bash = namespace "bash" $
  dotfiles $
    copy "configs/bash/bashrc" ".bashrc"

namespace_zsh :: Script 'Sources ()
namespace_zsh = namespace "zsh" $ do
  git (url "https://github.com/zsh-users/zsh-completions" . path "git/zsh-completions") pass
  git (url "https://github.com/zsh-users/zsh-syntax-highlighting" . path "git/zsh-syntax-highlighting") pass
  dotfiles $ do
    copy "configs/zsh/zshrc" ".zshrc"
    copy "configs/zsh/zshenv" ".zshenv"
    copy "configs/zsh/zshnix" ".zshnix"
    copy "configs/zsh/zprofile" ".zprofile"

namespace_java :: Script 'Sources ()
namespace_java = namespace "idea" $
  dotfiles $
    copy "configs/idea/config/colors/bleedie.xml" ".IdeaIC12/config/colors/bleedie.xml"

namespace_conky :: Script 'Sources ()
namespace_conky = namespace "conky" $
  dotfiles $
    copy "configs/conky/conkyrc" ".conkyrc"

namespace_gtk :: Script 'Sources ()
namespace_gtk = namespace "gtk" $
  dotfiles $
    copy "configs/gtk/gtkrc-2.0.mine" ".gtkrc-2.0.mine"

namespace_tmux :: Script 'Sources ()
namespace_tmux = namespace "tmux" $ do
  git (url "https://github.com/tmux-plugins/tpm" . path ".tmux/plugins/tpm") pass
  dotfiles $ do
    substitute "configs/tmux/tmux.conf.template" ".tmux.conf"
    copy "configs/tmux/tmux.keybindings" ".tmux.keybindings"

namespace_uzbl :: Script 'Sources ()
namespace_uzbl = namespace "uzbl" $
  dotfiles $
    copy "configs/uzbl/config" ".config/uzbl/config"

namespace_dwb :: Script 'Sources ()
namespace_dwb = namespace "dwb" $
  dotfiles $ do
    copy "configs/dwb/settings" ".config/dwb/settings"
    copy "configs/dwb/keys" ".config/dwb/keys"

namespace_vifm :: Script 'Sources ()
namespace_vifm = namespace "vifm" $
  dotfiles $ do
    copy "configs/vifm/colors/hybrid" ".vifm/colors/hybrid"
    copy "configs/vifm/vifmrc" ".vifm/vifmrc"

namespace_zathura :: Script 'Sources ()
namespace_zathura = namespace "zathura" $
  dotfiles $
    copy "configs/zathura/zathurarc" ".config/zathura/zathurarc"
