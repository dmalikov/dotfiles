#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}
import Biegunka
import Biegunka.Source.Git

main ∷ IO ()
main = execute $
  profile "mine" $ do
    dotfiles
    vim_scala

links = mapM_ $ uncurry link

dotfiles ∷ SourceScript () () ()
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
  ]

vim_scala = git "git@github.com:scala/scala-dist" "dmalikov/scala-dist" $ links
  [ ( "tool-support/src/vim/ftdetect/scala.vim", ".vim/ftdetect/scala.vim" )
  , ( "tool-support/src/vim/indent/scala.vim", ".vim/indent/scala.vim" )
  , ( "tool-support/src/vim/syntax/scala.vim", ".vim/syntax/scala.vim" )
  ]
