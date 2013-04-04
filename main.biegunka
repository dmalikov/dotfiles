{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE UnicodeSyntax #-}
import           Control.Lens
import           Data.Default          (def)

import           Biegunka
import           Biegunka.Source.Darcs
import           Biegunka.Source.Git

main ∷ IO ()
main =
  biegunka (set root "~") profiles $ execute def
 where
  profiles = sequence_
    [ vim
    , xmonad
    , git'
    , ruby
    , x
    , ghc
    , irssi
    , mpd
    , mplayer
    , pentadactyl
    , screen
    , ackrc
    , apvlv
    , shell'
    , java
    , conky
    , gtk
    , mocp
    , tmux
    , uzbl
    , vifm
    , misc
    ]

git_dotfiles = git "git@github.com:dmalikov/dotfiles" "projects/dmalikov/dotfiles"

vim :: Script Profiles
vim = do
  profile "vim/pathogen/meta" $ do
    git "git@github.com:tpope/vim-pathogen.git" "projects/misc/vim-pathogen" $
      link "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
  profile "vim/pathogen/modules" $ do
    git "git@github.com:Shougo/vimproc.git" ".vim/bundle/vimproc" $
      shell "make -f make_unix.mak"
    git_ "git@github.com:Shougo/neocomplcache.git" ".vim/bundle/neocomplcache"
    git_ "git@github.com:Shougo/unite.vim.git" ".vim/bundle/unite"
    git_ "git@github.com:airblade/vim-gitgutter.git" ".vim/bundle/gitgutter"
    git_ "git@github.com:dahu/Insertlessly.git" ".vim/bundle/Insertlessly"
    git_ "git@github.com:godlygeek/tabular.git" ".vim/bundle/tabular"
    git_ "git@github.com:scrooloose/syntastic.git" ".vim/bundle/syntastic"
    git_ "git@github.com:spolu/dwm.vim.git" ".vim/bundle/dwm"
    git_ "git@github.com:supki/vim-perds.git" ".vim/bundle/perds"
    git_ "git@github.com:tpope/vim-commentary.git" ".vim/bundle/commentary"
    git_ "git@github.com:tpope/vim-markdown.git" ".vim/bundle/markdown"
    git_ "git@github.com:tpope/vim-surround.git" ".vim/bundle/surround"
    git_ "git@github.com:ujihisa/neco-ghc.git" ".vim/bundle/neco-ghc"
  profile "vim/rc" $
    git_dotfiles $ link "vimrc" ".vimrc"
  profile "vim/syntax" $
    git_dotfiles $ link "haskell.vim" ".vim/after/syntax/haskell.vim"
  profile "vim/colorschemes" $
    git_dotfiles $ link "neverland-darker.vim" ".vim/colors/neverland-darker.vim"

xmonad :: Script Profiles
xmonad = do
  profile "xmonad/darcs/repos" $ do
    darcs_ "http://code.haskell.org/xmonad" "projects/misc/xmonad"
    darcs_ "http://code.haskell.org/XMonadContrib" "projects/misc/xmonad-contrib"
    darcs_ "http://code.haskell.org/xmonad-extras/" "projects/misc/xmonad-extras"
  profile "xmonad/xmonad.hs" $
    git_dotfiles $ link "xmonad" ".xmonad/xmonad.hs"
  profile "xmonad/xmobar" $ do
    git_dotfiles $ do
      link "xmobar-top.hs" ".xmonad/xmobar-top.hs"
      link "xmobar.hs" ".xmonad/xmobar.hs"
      link "xmobarrc" ".xmobarrc"

git' :: Script Profiles
git' = profile "git" $ do
  git_dotfiles $ do
    link "gitconfig" ".gitconfig"
    link "gitignore" ".gitignore"
    link "tigrc" ".tigrc"
  git_ "git@github.com:nvie/gitflow.git" "projects/misc/gitflow"

ruby :: Script Profiles
ruby = profile "ruby" $ do
  git_dotfiles $ do
    link "irbrc" ".irbrc"
    link "rvmrc" ".rvmrc"

x :: Script Profiles
x = profile "X" $ do
  git_dotfiles $ do
    link "XCompose" ".XCompose"
    link "Xdefaults" ".Xdefaults"
    link "Xresources.large" ".Xresources.large"
    link "Xresources.scratchpad" ".Xresources.scratchpad"
    link "Xresources.shiva" ".Xresources.shiva"
    link "inputrc" ".inputrc"
    link "prexinit" "prexinit"
    link "xinitrc" ".xinitrc"

ghc :: Script Profiles
ghc = profile "ghc" $ do
  git_dotfiles $
    link "ghci" ".ghci"
  git_ "git@github.com:eagletmt/ghcmod-vim.git" ".vim/bundle/ghcmod-vim"
  git_ "git@github.com:bitc/vim-hdevtools.git" ".vim/bundle/hdevtools"

irssi :: Script Profiles
irssi = profile "irssi" $
  git_dotfiles $ do
    link "irssi.bleeding.theme" ".irssi/bleeding.theme"
    link "irssi.config" ".irssi/config"

mpd :: Script Profiles
mpd = profile "mpd" $
  git_dotfiles $ do
    link "mpdconf" ".mpdconf"
    link "ncmpcppconf" ".ncmpcpp/config"

mplayer :: Script Profiles
mplayer = profile "mplayer" $
  git_dotfiles $ do
    link "mplayer.config" ".mplayer/config"
    link "mplayer.input.conf" ".mplayer/input.conf"

pentadactyl :: Script Profiles
pentadactyl = profile "pentadactyl" $
  git_dotfiles $ do
    link "pemees.penta" ".pentadactyl/colors/pemees.penta"
    link "pentadactylrc" ".pentadactylrc"
    link "buftabs.js" ".pentadactyl/plugins/buftabs.js"

screen :: Script Profiles
screen = profile "screen" $
  git_dotfiles $
    link "screenrc" ".screenrc"

ackrc :: Script Profiles
ackrc = profile "ack" $
  git_dotfiles $
    link "ackrc" ".ackrc"

apvlv :: Script Profiles
apvlv = profile "apvlv" $
  git_dotfiles $
    link "apvlvrc" ".apvlvrc"

shell' :: Script Profiles
shell' = do
  profile "shell/bash" $
    git_dotfiles $
      link "bashrc" ".bashrc"
  profile "shell/zsh" $
    git_dotfiles $
      link "zshrc" ".zshrc"

java :: Script Profiles
java = do
  profile "idea" $
    git_dotfiles $
      link "bleedie.xml" ".IdeaIC12/config/colors/bleedie.xml"

conky :: Script Profiles
conky = profile "conky" $
  git_dotfiles $
    link "conkyrc" ".conkyrc"

gtk :: Script Profiles
gtk = profile "gtk" $
  git_dotfiles $
    link "gtkrc-2.0.mine" ".gtkrc-2.0.mine"

mocp :: Script Profiles
mocp = profile "mocp" $
  git_dotfiles $
    link "mocpconfig" ".moc/config"

tmux :: Script Profiles
tmux = profile "tmux" $
  git_dotfiles $
    link "tmux.conf" ".tmux.conf"

uzbl :: Script Profiles
uzbl = profile "uzbl" $
  git_dotfiles $
    link "uzbl-config" ".config/uzbl/config"

vifm :: Script Profiles
vifm = profile "vifm" $
  git_dotfiles $ do
    link "vifm-neverland.colorscheme" ".vifm/colors/neverland"
    link "vifmrc" ".vifm/vifmrc"

misc :: Script Profiles
misc = do
  profile "misc/urxvt/tabbedex" $
    git "git@github.com:stepb/urxvt-tabbedex.git" "projects/misc/urxvt-tabbedex" $
      link "tabbedex" ".urxvt/perl/tabbedex"
  profile "misc/hpasteit" $
    git_ "git@github.com:parcs/hpasteit.git" "projects/misc/hpasteit"
