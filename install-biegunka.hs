#!/usr/bin/runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
import System.FilePath.Posix

import Biegunka
import Biegunka.Source.Git

main ∷ IO ()
main = execute $
  profile "mine" $ do
    dotfiles
    vimPathogen
    vimPathogenModules

dotfiles ∷ Script Source
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
  , ( "xmobarrc", ".xmobarrc" )
  , ( "xmonad.hs", ".xmonad/xmonad.hs" )
  , ( "uzbl-config", ".config/uzbl/config"
  ]

vimPathogen = git "git@github.com:tpope/vim-pathogen.git" "dmalikov/vim-pathogen" $
  link "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"

vimPathogenModules = mapM_ pathogenModule
  [ "git@github.com:rosstimson/scala-vim-support"
  , "git@github.com:scrooloose/nerdtree.git"
  , "git@github.com:scrooloose/syntastic.git"
  , "git@github.com:Shougo/vimproc.git"
  , "git@github.com:eagletmt/ghcmod-vim.git"
  , "git@github.com:ujihisa/neco-ghc.git"
  , "git@github.com:Shougo/neocomplcache.git"
  , "git@github.com:tpope/vim-surround.git"
  , "git@github.com:tpope/vim-markdown.git"
  , "git@github.com:Shougo/unite.vim.git"
  ]

pathogenModule gitLink = git gitLink ("dmalikov" </> projectName) $
  link "." $ joinPath [ ".vim", "bundle", projectName ]
    where projectName = takeFileName $ dropExtension gitLink

urxvtTabbedex = git "git@github.com:stepb/urxvt-tabbedex.git" "dmalikov/urxvt-tabbedex" $
  link "tabbedex" ".urxvt/perl/tabbedex"

links = mapM_ $ uncurry link
copys = mapM_ $ uncurry copy
