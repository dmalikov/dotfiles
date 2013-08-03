{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module Profiles where
import           Control.Lens
import           Data.Default                (def)

import           Control.Biegunka
import           Control.Biegunka.Source.Git

dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:dmalikov/dotfiles" "projects/dmalikov/dotfiles" $ def & actions .~ as

pathogenize repo = git ("git@github.com:" ++ repo) (".vim/bundle/")
pathogenize_ repo = pathogenize repo (return ())

profile_vim :: Script Sources ()
profile_vim = do
  profile "vim/pathogen/meta" $ do
    git "git@github.com:tpope/vim-pathogen.git" "projects/misc/vim-pathogen" $
      copy "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
  profile "vim/pathogen/modules" $ do
    pathogenize "Shougo/vimproc" $
      shell "make -f make_unix.mak"
    mapM pathogenize_
      [ "Shougo/neocomplcache"
      , "Shougo/unite.vim"
      , "airblade/vim-gitgutter"
      , "dahu/Insertlessly"
      , "godlygeek/tabular"
      , "scrooloose/syntastic"
      , "spolu/dwm.vim"
      , "supki/vim-perds"
      , "tpope/vim-commentary"
      , "tpope/vim-markdown"
      , "tpope/vim-surround"
      , "ujihisa/neco-ghc"
      , "jvoorhis/coq.vim"
      , "trefis/coquille"
      , "def-lkb/vimbufsync"
      , "suan/vim-instant-markdown"
      , "merlinrebrovic/focus.vim"
      , "vim-scripts/YankRing.vim"
      , "derekwyatt/vim-sbt"
      , "takac/vim-hardtime"
      ]
  profile "vim/rc" $
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  profile "vim/syntax" $
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
  profile "vim/colorschemes" $
    dotfiles $ copy "configs/vim/colors/neverland-darker.vim" ".vim/colors/neverland-darker.vim"


profile_xmonad :: Script Sources ()
profile_xmonad = do
  profile "xmonad/xmonad.hs" $
    dotfiles $
      substitute "configs/xmonad/xmonad.hs.template" ".xmonad/xmonad.hs"
  profile "xmonad/xmobar" $ do
    git "git@github.com:dmalikov/xmobar-usable" "projects/dmalikov/xmobar-usable" $
      shell "cabal install --flags=\"all_extensions\""
    dotfiles $ do
      copy "configs/xmonad/xmobar.hs" ".xmonad/xmobar.hs"
      shell "ghc -O2 ${HOME}/.xmonad/xmobar.hs -o ${HOME}/.xmonad/xmobar -fforce-recomp"

profile_git :: Script Sources ()
profile_git = profile "git" $ do
  dotfiles $ do
    substitute "configs/git/config.template" ".gitconfig"
    copy "configs/git/ignore" ".gitignore"
    copy "configs/git/tigrc" ".tigrc"
  git_ "git@github.com:nvie/gitflow.git" "projects/misc/gitflow"
    -- install?
  git_ "git@github.com:arc90/git-sweep.git" "projects/misc/git-sweep"
    -- install?

profile_ruby :: Script Sources ()
profile_ruby = profile "ruby" $ do
  dotfiles $ do
    copy "configs/ruby/irbrc" ".irbrc"
    copy "configs/ruby/rvmrc" ".rvmrc"

profile_x :: Script Sources ()
profile_x = profile "X" $ do
  dotfiles $ do
    copy "configs/X/XCompose" ".XCompose"
    substitute "configs/X/Xresources.template" ".Xresources"
    copy "configs/X/Xresources.large" ".Xresources.large"
    substitute "configs/X/Xresources.scratchpad.template" ".Xresources.scratchpad"
    substitute "configs/X/Xresources.shiva.template" ".Xresources.shiva"
    copy "configs/X/inputrc" ".inputrc"
    copy "configs/X/prexinit" ".prexinit"
    copy "configs/X/xinitrc" ".xinitrc"

profile_ghc :: Script Sources ()
profile_ghc = profile "ghc" $ do
  mapM pathogenize_ ["eagletmt/ghcmod-vim", "bitc/vim-hdevtools"]
  dotfiles $ do
    copy "configs/ghc/ghci" ".ghci"
    copy "configs/ghc/stylish-haskell.yaml" ".stylish-haskell.yaml"

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
  profile "shell/bash" $
    dotfiles $
      copy "configs/shell/bash/bashrc" ".bashrc"
  profile "shell/zsh" $
    dotfiles $ do
      copy "configs/shell/zsh/zshrc" ".zshrc"
      copy "configs/shell/zsh/zprofile" ".zprofile"

profile_java :: Script Sources ()
profile_java = do
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
  dotfiles $
    copy "configs/tmux/conf" ".tmux.conf"

profile_uzbl :: Script Sources ()
profile_uzbl = profile "uzbl" $
  dotfiles $
    copy "configs/uzbl/config" ".config/uzbl/config"

profile_vifm :: Script Sources ()
profile_vifm = profile "vifm" $
  dotfiles $ do
    copy "configs/vifm/colors/neverland" ".vifm/colors/neverland"
    copy "configs/vifm/vifmrc" ".vifm/vifmrc"

profile_zathura :: Script Sources ()
profile_zathura = profile "zathura" $
  dotfiles $
    copy "configs/zathura/zathurarc" ".config/zathura/zathurarc"
