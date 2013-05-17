{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE UnicodeSyntax #-}
module Profiles where
import           Control.Lens
import           Data.Default          (def)

import           Biegunka
import           Biegunka.Source.Git

{-
main ∷ IO ()
main =
  biegunka (set root "~") profiles $ execute def
-}

dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:dmalikov/dotfiles" "projects/dmalikov/dotfiles" $ def & actions .~ as & branch .~ "feature/templating"

profile_vim :: Script Profiles ()
profile_vim = do
  profile "vim/pathogen/meta" $ do
    git "git@github.com:tpope/vim-pathogen.git" "projects/misc/vim-pathogen" $
      copy "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
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
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  profile "vim/syntax" $
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
  profile "vim/colorschemes" $
    dotfiles $ copy "configs/vim/colors/neverland-darker.vim" ".vim/colors/neverland-darker.vim"

profile_xmonad :: Script Profiles ()
profile_xmonad = do
  profile "xmonad/xmonad.hs" $
    dotfiles $
      copy "configs/xmonad/xmonad.hs" ".xmonad/xmonad.hs"
  profile "xmonad/xmobar" $ do
    dotfiles $ do
      copy "configs/xmonad/xmobar-top.hs" ".xmonad/xmobar-top.hs"
      copy "configs/xmonad/xmobar.hs" ".xmonad/xmobar.hs"
      copy "configs/xmonad/xmobarrc" ".xmobarrc"

profile_git :: Script Profiles ()
profile_git = profile "git" $ do
  dotfiles $ do
    copy "configs/git/config" ".gitconfig"
    copy "configs/git/ignore" ".gitignore"
    copy "configs/git/tigrc" ".tigrc"
  git_ "git@github.com:nvie/gitflow.git" "projects/misc/gitflow"
    -- install?
  git_ "git@github.com:arc90/git-sweep.git" "projects/misc/git-sweep"
    -- install?

profile_ruby :: Script Profiles ()
profile_ruby = profile "ruby" $ do
  dotfiles $ do
    copy "configs/ruby/irbrc" ".irbrc"
    copy "configs/ruby/rvmrc" ".rvmrc"

profile_x :: Script Profiles ()
profile_x = profile "X" $ do
  dotfiles $ do
    copy "configs/X/XCompose" ".XCompose"
    copy "configs/X/Xdefaults" ".Xdefaults"
    copy "configs/X/Xresources.large" ".Xresources.large"
    copy "configs/X/Xresources.scratchpad" ".Xresources.scratchpad"
    copy "configs/X/Xresources.shiva" ".Xresources.shiva"
    copy "configs/X/inputrc" ".inputrc"
    copy "configs/X/prexinit" "prexinit"
    copy "configs/X/xinitrc" ".xinitrc"

profile_ghc :: Script Profiles ()
profile_ghc = profile "ghc" $ do
  dotfiles $
    copy "configs/ghc/ghci" ".ghci"
  git_ "git@github.com:eagletmt/ghcmod-vim.git" ".vim/bundle/ghcmod-vim"
  git_ "git@github.com:bitc/vim-hdevtools.git" ".vim/bundle/hdevtools"

profile_irssi :: Script Profiles ()
profile_irssi = profile "irssi" $
  dotfiles $ do
    copy "configs/irssi/bleeding.theme" ".irssi/bleeding.theme"
    copy "configs/irssi/config" ".irssi/config"

profile_mpd :: Script Profiles ()
profile_mpd = profile "mpd" $
  dotfiles $ do
    copy "configs/mpd/mpdconf" ".mpdconf"
    copy "configs/mpd/ncmpcpp/config" ".ncmpcpp/config"

profile_mplayer :: Script Profiles ()
profile_mplayer = profile "mplayer" $
  dotfiles $ do
    copy "configs/mplayer/config" ".mplayer/config"
    copy "configs/mplayer/input.conf" ".mplayer/input.conf"

profile_pentadactyl :: Script Profiles ()
profile_pentadactyl = profile "pentadactyl" $
  dotfiles $ do
    copy "configs/pentadactyl/colors/pemees.penta" ".pentadactyl/colors/pemees.penta"
    copy "configs/pentadactyl/pentadactylrc" ".pentadactylrc"
    copy "configs/pentadactyl/plugins/buftabs.js" ".pentadactyl/plugins/buftabs.js"

profile_screen :: Script Profiles ()
profile_screen = profile "screen" $
  dotfiles $
    copy "configs/screen/screenrc" ".screenrc"

profile_ackrc :: Script Profiles ()
profile_ackrc = profile "ack" $
  dotfiles $
    copy "configs/ack/ackrc" ".ackrc"

profile_apvlv :: Script Profiles ()
profile_apvlv = profile "apvlv" $
  dotfiles $
    copy "configs/apvlv/apvlvrc" ".apvlvrc"

profile_shell :: Script Profiles ()
profile_shell = do
  profile "shell/bash" $
    dotfiles $
      copy "configs/shell/bash/bashrc" ".bashrc"
  profile "shell/zsh" $
    dotfiles $
      copy "configs/shell/zsh/zshrc" ".zshrc"

profile_java :: Script Profiles ()
profile_java = do
  profile "idea" $
    dotfiles $
      copy "configs/idea/config/colors/bleedie.xml" ".IdeaIC12/config/colors/bleedie.xml"

profile_conky :: Script Profiles ()
profile_conky = profile "conky" $
  dotfiles $
    copy "configs/conky/conkyrc" ".conkyrc"

profile_gtk :: Script Profiles ()
profile_gtk = profile "gtk" $
  dotfiles $
    copy "configs/gtk/gtkrc-2.0.mine" ".gtkrc-2.0.mine"

profile_mocp :: Script Profiles ()
profile_mocp = profile "mocp" $
  dotfiles $
    copy "configs/moc/config" ".moc/config"

profile_tmux :: Script Profiles ()
profile_tmux = profile "tmux" $
  dotfiles $
    copy "configs/tmux/conf" ".tmux.conf"

profile_uzbl :: Script Profiles ()
profile_uzbl = profile "uzbl" $
  dotfiles $
    copy "configs/uzbl/config" ".config/uzbl/config"

profile_vifm :: Script Profiles ()
profile_vifm = profile "vifm" $
  dotfiles $ do
    copy "configs/vifm/colors/neverland" ".vifm/colors/neverland"
    copy "configs/vifm/vifmrc" ".vifm/vifmrc"

profile_misc :: Script Profiles ()
profile_misc = do
  profile "misc/hpasteit" $
    git_ "git@github.com:parcs/hpasteit.git" "projects/misc/hpasteit"
