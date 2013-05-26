# dotfiles

World of configuration files managed by [`biegunka`][biegunka].

[![Build Status](https://drone.io/github.com/dmalikov/dotfiles/status.png)](https://drone.io/github.com/dmalikov/dotfiles/latest)

## What is it?

This is [`biegunka`][biegunka]-powered repository containing my configuration
files. It has nice mechanism for maintain many working environments with
different configurations.

## Why biegunka?

Usual `install.sh` script sucks when number of working environments that should
be maintained is more than 1. Simple scripts sucks when you want to synchronize
git repositories regulary. All these reinvent-the-wheel scripts suck when you
want to clean all shit out of your system, that your installation script
produced.

[`Chef`][chef] and
[`chef-solo`][chef-solo] could be helpful here
with all these recipes and roles power, but I'm too lazy for writing a cookbook
for every application that I have.

[`biegunka`][biegunka] could be used not just for dummy copying files to
specific directories, but for syncing git or darcs repositories. It is quite
usable when you have many repositories and almost die of boredom updating them
every day.  It could be successfully done by another `sh` script, but
[`biegunka`][biegunka] offers nice readable declarative syntax and beatiful
methods to formalize all that logic nicely.

## How I'm using it?

`dotfiles` is a simple `cabal` package. So, at first, it should be compiled to
something executable:

```
$> cabal install biegunka/dotfiles.cabal
```

Currenty I have only 2 active laptops managed by biegunka - `x220` and `t510`
(these names is used as identifiers for each of the `Environment`s). I'm
updating my `github:dotfiles` repository from time to time, and biegunka helps
here that each of working station could have up to date configuration.

Dotfiles installation with `x220` profile:

```
$> dotfiles -e x220
```

`pretend` flag shows what [`biegunka`][biegunka] gonna install:

```
$> dotfiles -e x220 --pretend
Print full log? [yN]: y
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.ackrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/ack/ackrc
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.ghci is a copy of /home/m/projects/dmalikov/dotfiles/configs/ghc/ghci
update git source git@github.com:eagletmt/ghcmod-vim.git at /home/m/.vim/bundle/ghcmod-vim
update git source git@github.com:bitc/vim-hdevtools.git at /home/m/.vim/bundle/hdevtools
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.gitconfig is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/git/config.template
  /home/m/.gitignore is a copy of /home/m/projects/dmalikov/dotfiles/configs/git/ignore
  /home/m/.tigrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/git/tigrc
update git source git@github.com:nvie/gitflow.git at /home/m/projects/misc/gitflow
update git source git@github.com:arc90/git-sweep.git at /home/m/projects/misc/git-sweep
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.gtkrc-2.0.mine is a copy of /home/m/projects/dmalikov/dotfiles/configs/gtk/gtkrc-2.0.mine
update git source git@github.com:parcs/hpasteit.git at /home/m/projects/misc/hpasteit
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.mpdconf is a copy of /home/m/projects/dmalikov/dotfiles/configs/mpd/mpdconf
  /home/m/.ncmpcpp/config is a copy of /home/m/projects/dmalikov/dotfiles/configs/mpd/ncmpcpp/config
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.mplayer/config is a copy of /home/m/projects/dmalikov/dotfiles/configs/mplayer/config
  /home/m/.mplayer/input.conf is a copy of /home/m/projects/dmalikov/dotfiles/configs/mplayer/input.conf
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.pentadactyl/colors/pemees.penta is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/pentadactyl/colors/pemees.penta.template
  /home/m/.pentadactylrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/pentadactyl/pentadactylrc
  /home/m/.pentadactyl/plugins/buftabs.js is a copy of /home/m/projects/dmalikov/dotfiles/configs/pentadactyl/plugins/buftabs.js
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.irbrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/ruby/irbrc
  /home/m/.rvmrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/ruby/rvmrc
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.bashrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/shell/bash/bashrc
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.zshrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/shell/zsh/zshrc
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.tmux.conf is a copy of /home/m/projects/dmalikov/dotfiles/configs/tmux/conf
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.config/uzbl/config is a copy of /home/m/projects/dmalikov/dotfiles/configs/uzbl/config
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.vifm/colors/neverland is a copy of /home/m/projects/dmalikov/dotfiles/configs/vifm/colors/neverland
  /home/m/.vifm/vifmrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/vifm/vifmrc
update git source git@github.com:tpope/vim-pathogen.git at /home/m/projects/misc/vim-pathogen
  /home/m/.vim/autoload/pathogen.vim is a copy of /home/m/projects/misc/vim-pathogen/autoload/pathogen.vim
update git source git@github.com:Shougo/vimproc.git at /home/m/.vim/bundle/vimproc
  shell `make -f make_unix.mak` executed from /home/m/.vim/bundle/vimproc
update git source git@github.com:Shougo/neocomplcache.git at /home/m/.vim/bundle/neocomplcache
update git source git@github.com:Shougo/unite.vim.git at /home/m/.vim/bundle/unite
update git source git@github.com:airblade/vim-gitgutter.git at /home/m/.vim/bundle/gitgutter
update git source git@github.com:dahu/Insertlessly.git at /home/m/.vim/bundle/Insertlessly
update git source git@github.com:godlygeek/tabular.git at /home/m/.vim/bundle/tabular
update git source git@github.com:scrooloose/syntastic.git at /home/m/.vim/bundle/syntastic
update git source git@github.com:spolu/dwm.vim.git at /home/m/.vim/bundle/dwm
update git source git@github.com:supki/vim-perds.git at /home/m/.vim/bundle/perds
update git source git@github.com:tpope/vim-commentary.git at /home/m/.vim/bundle/commentary
update git source git@github.com:tpope/vim-markdown.git at /home/m/.vim/bundle/markdown
update git source git@github.com:tpope/vim-surround.git at /home/m/.vim/bundle/surround
update git source git@github.com:ujihisa/neco-ghc.git at /home/m/.vim/bundle/neco-ghc
update git source git@github.com:jvoorhis/coq.vim.git at /home/m/.vim/bundle/coq
update git source git@github.com:trefis/coquille.git at /home/m/.vim/bundle/coquille
update git source git@github.com:def-lkb/vimbufsync.git at /home/m/.vim/bundle/bufsync
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.vimrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/vim/vimrc
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.vim/after/syntax/haskell.vim is a copy of /home/m/projects/dmalikov/dotfiles/configs/vim/syntax/haskell.vim
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.vim/colors/neverland-darker.vim is a copy of /home/m/projects/dmalikov/dotfiles/configs/vim/colors/neverland-darker.vim
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.XCompose is a copy of /home/m/projects/dmalikov/dotfiles/configs/X/XCompose
  /home/m/.Xdefaults is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/X/Xdefaults.template
  /home/m/.Xresources.large is a copy of /home/m/projects/dmalikov/dotfiles/configs/X/Xresources.large
  /home/m/.Xresources.scratchpad is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/X/Xresources.scratchpad.template
  /home/m/.Xresources.shiva is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/X/Xresources.shiva.template
  /home/m/.inputrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/X/inputrc
  /home/m/prexinit is a copy of /home/m/projects/dmalikov/dotfiles/configs/X/prexinit
  /home/m/.xinitrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/X/xinitrc
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.xmonad/xmonad.hs is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmonad.hs.template
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.xmonad/xmobar-top.hs is a copy of /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmobar-top.hs
  /home/m/.xmonad/xmobar.hs is a copy of /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmobar.hs
  /home/m/.xmobarrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmobarrc

Press any key to continue
```
This is a full log of what [`biegunka`][biegunka] is preparing to do. It is
kinda usable when you're managing some important files or it is a first
[`biegunka`][biegunka] run on experementative machine.


There is another useful command named `verify`:
```
$> dotfiles --verify -e x220
...
[localhost] (git@github.com:tpope/vim-surround.git) update git source at /home/m/.vim/bundle/surround
[localhost] (git@github.com:ujihisa/neco-ghc.git) update git source at /home/m/.vim/bundle/neco-ghc
[localhost] (git@github.com:jvoorhis/coq.vim.git) update git source at /home/m/.vim/bundle/coq
[localhost] (git@github.com:trefis/coquille.git) update git source at /home/m/.vim/bundle/coquille
[localhost] (git@github.com:def-lkb/vimbufsync.git) update git source at /home/m/.vim/bundle/bufsync
Verification:
OK
```
So this `verify` command force [`biegunka`][biegunka] to check that all actions
defined in `Profiles` is done. `--verify` and `--pretend` options could be used
the same time.

Configuring dotfiles on the other working station (`t510`) looks similar:

```
$> dotfiles --pretend --verify -e t510
...

```

## How it works?

`dotfiles` package consist of `Main.hs`, `Profiles.hs` and `Environment/*`
modules.

`Main.hs` is a main module. It contains command-line options parser and
`biegunka` function call:

```haskell
biegunka (set root "~") (pretend' <> execute (set templates $ Templates settings') <> verify') profiles'
```

`profiles` is a variable of `:: Script Profiles ()` type. It contains all
information about what _profiles_ will be installed.

`Profiles.hs` file contains all profiles that I have. Here is example of xmonad
profile:

```haskell
profile_xmonad :: Script Profiles ()
profile_xmonad = do
  profile "xmonad/xmonad.hs" $
    dotfiles $
      substitute "configs/xmonad/xmonad.hs.template" ".xmonad/xmonad.hs"
  profile "xmonad/xmobar" $ do
    dotfiles $ do
      copy "configs/xmonad/xmobar-top.hs" ".xmonad/xmobar-top.hs"
      copy "configs/xmonad/xmobar.hs" ".xmonad/xmobar.hs"
      copy "configs/xmonad/xmobarrc" ".xmobarrc"
```

`dotfiles` is a function stands for a link to my dotfile repository:

``` haskell
dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:dmalikov/dotfiles" "projects/dmalikov/dotfiles" $ def & actions .~ as
```

`copy` action take given file and put it to the specific place.

`substitute` action do the same thing but with completing all template
variables for template.

`settings` is a variable of `:: Template` type. It contains all necessary
template variables that completes all templates holes.

There are 2 environments in `Environment` directory. Each environment specifies
actions and template variables for completing templates hole.

For example, suppose I need to have `user.name` and `user.email` git options in
`~/.gitconfig` file only in one environment. `gitconfig` template looks like
that:

```
$if(template.git.set_user)$
[user]
    name = $template.git.user_name$
    email = $template.git.user_email$
$endif$
```

`X220` environment contains definition for these template placeholders:

```haskell
settings :: Template
settings = def
  { git = def
    { set_user = True
    , user_name = "Dmitry Malikov"
    , user_email = "malikov.d.y@gmail.com"
    }
...
```

`T510` environment doesn't have these variables and `set_user` is equal to
`False` for it.

`git` profiles has these call:

```
substitute "configs/git/config.template" ".gitconfig"
```

So it produces `[user]` part of gitconfig only for `X220` environment:

```
[user]
    name = Dmitry Malikov
    email = malikov.d.y@gmail.com
```

[biegunka]: https://github.com/biegunka
[chef]: https://github.com/opscode/chef
[chef-solo]: http://docs.opscode.com/chef_solo.html
