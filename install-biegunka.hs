#!/usr/bin/runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
import System.FilePath.Posix

import Biegunka
import Biegunka.Source.Git
import Biegunka.Source.Darcs

main ∷ IO ()
main = execute $
  profile "mine" $ do
    dotfiles
    gitflow
    hpasteit
    urxvt_tabbedex
    vim_pathogen
    vim_pathogen_modules
    xmonad


dotfiles = git "git@github.com:dmalikov/dotfiles" "dmalikov/dotfiles" $ links
  [ ( "ackrc", ".ackrc" )
  , ( "apvlvrc", ".apvlvrc" )
  , ( "bashrc", ".bashrc" )
  , ( "buftabs.js", ".pentadactyl/plugins/buftabs.js" )
  , ( "conkyrc", ".conkyrc" )
  , ( "ghci", ".ghci" )
  , ( "gitconfig", ".gitconfig" )
  , ( "gitignore", ".gitignore" )
  , ( "gtkrc-2.0.mine", ".gtkrc-2.0.mine" )
  , ( "haskell.vim", ".vim/after/syntax/haskell.vim" )
  , ( "irssi.bleeding.theme", ".irssi/bleeding.theme" )
  , ( "irssi.config", ".irssi/config" )
  , ( "mocpconfig", ".moc/config" )
  , ( "mpdconf", ".mpdconf" )
  , ( "neverland-darker.vim", ".vim/colors/neverland-darker.vim" )
  , ( "ncmpcppconf", ".ncmpcpp/config" )
  , ( "pemees.penta", ".pentadactyl/colors/pemees.penta" )
  , ( "pentadactylrc", ".pentadactylrc" )
  , ( "prexinit", "prexinit" )
  , ( "screenrc", ".screenrc" )
  , ( "Xresources.scratchpad", ".Xresources.scratchpad" )
  , ( "Xresources.shiva", ".Xresources.shiva" )
  , ( "tigrc", ".tigrc" )
  , ( "tmux.conf", ".tmux.conf" )
  , ( "vimrc", ".vimrc" )
  , ( "XCompose", ".XCompose" )
  , ( "Xdefaults", ".Xdefaults" )
  , ( "xinitrc", ".xinitrc" )
  , ( "xmobar.hs", ".xmonad/xmobar.hs" )
  , ( "xmobar-top.hs", ".xmonad/xmobar-top.hs" )
  , ( "xmobarrc", ".xmobarrc" )
  , ( "xmonad.hs", ".xmonad/xmonad.hs" )
  , ( "uzbl-config", ".config/uzbl/config" )
  , ( "zshrc", ".zshrc" )
  ]

vim_pathogen = git "git@github.com:tpope/vim-pathogen.git" "dmalikov/vim-pathogen" $
  link "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"

vim_pathogen_modules = do
  git_ "git@github.com:rosstimson/scala-vim-support" ".vim/bundle/scala-support"
  git_ "git@github.com:scrooloose/syntastic.git" ".vim/bundle/syntastic"
  git "git@github.com:Shougo/vimproc.git" ".vim/bundle/vimproc" $ shell "make -f make_unix.mak"
  git_ "git@github.com:eagletmt/ghcmod-vim.git" ".vim/bundle/ghcmod-vim"
  git_ "git@github.com:ujihisa/neco-ghc.git" ".vim/bundle/neco-ghc"
  git_ "git@github.com:Shougo/neocomplcache.git" ".vim/bundle/neocomplcache"
  git_ "git@github.com:tpope/vim-surround.git" ".vim/bundle/surround"
  git_ "git@github.com:tpope/vim-markdown.git" ".vim/bundle/markdown"
  git_ "git@github.com:Shougo/unite.vim.git" ".vim/bundle/unite"
  git_ "git@github.com:bitc/vim-hdevtools.git" ".vim/bundle/hdevtools"
  git_ "git@github.com:spolu/dwm.vim.git" ".vim/bundle/dwm"
  git_ "git@github.com:dahu/Insertlessly.git" ".vim/bundle/Insertlessly"


urxvt_tabbedex = git "git@github.com:stepb/urxvt-tabbedex.git" "dmalikov/urxvt-tabbedex" $
  link "tabbedex" ".urxvt/perl/tabbedex"

gitflow = git_ "git@github.com:nvie/gitflow.git" "gitflow"
hpasteit = git_ "git@github.com:parcs/hpasteit.git" "projects/hpasteit"

xmonad = do
  darcs_ "http://code.haskell.org/xmonad" "projects/xmonad"
  darcs_ "http://code.haskell.org/XMonadContrib" "projects/XMonadContrib"


links = mapM_ $ uncurry link
copys = mapM_ $ uncurry copy
