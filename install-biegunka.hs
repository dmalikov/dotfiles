{-# LANGUAGE UnicodeSyntax #-}

import Biegunka

main ∷ IO ()
main = execute $
  profile "mine" $
    dotfiles

dotfiles ∷ Script Repository ()
dotfiles = git "git@github.com:dmalikov/dotfiles" "dmalikov/dotfiles" $ do
    mapM_ (uncurry link)
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
