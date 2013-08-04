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

And now `dotfiles` executable is ready to go:

```
$> dotfiles
Usage: dotfiles (--x220 | --macBookPro) ([--run] | [--safe-run] | [--full] | [--dry-run] | [--check])

Available options:
  -h,--help                Show this help text
  --x220                   Use x220 settings
  --macBookPro             Use macBookPro settings
  --run                    Do real run
  --safe-run               Do real run (after confirmation)
  --full                   Do dry run, real run (after confirmation) and then check results
  --dry-run                Do only dry run, do not touch anything
  --check                  Compare current filesystem state against script
```

Currenty there is only one active laptop managed by biegunka - `x220`
(this name is used as identifier for each of the `Environment`s). I'm
updating my `github:dotfiles` repository from time to time, and biegunka helps
here that each of working station could have up to date configuration.

Dotfiles installation with `x220` profile:

```
$> dotfiles --x220 --run
```

`--dry-run` flag shows what [`biegunka`][biegunka] gonna install:

```
$> dotfiles --x220 --dry-run
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.ackrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/ack/ackrc
...
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.xmonad/xmonad.hs is copied with substituted templates from /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmonad.hs.template
update git source git@github.com:dmalikov/dotfiles at /home/m/projects/dmalikov/dotfiles
  /home/m/.xmonad/xmobar-top.hs is a copy of /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmobar-top.hs
  /home/m/.xmonad/xmobar.hs is a copy of /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmobar.hs
  /home/m/.xmobarrc is a copy of /home/m/projects/dmalikov/dotfiles/configs/xmonad/xmobarrc

```
This is a full log of what [`biegunka`][biegunka] is preparing to do. It is
kinda usable when you're managing some important files or it is a first
[`biegunka`][biegunka] run on experementative machine.


There is another useful command named `check`:
```
$> dotfiles --x220 --check
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

## How it works?

`dotfiles` package consist of `Main.hs`, `Profiles.hs` and `Environment/'
modules.

`Main.hs` is a main module. It contains a very important `TH` call

```haskell
makeOptionParser ''Environment
```

which automatically generate all necessary cmdline options and stuff.

```haskell
  (env, r) <- optionsParser
  case env of
    X220 -> r (set root "~" . set templates (hStringTemplate X220.settings)) X220.profiles
```

`profiles` is a variable of `:: Script Sources ()` type. It contains all
information about what _profiles_ will be installed.

`Profiles.hs` file contains all profiles that I have. Here is example of xmonad
profile:

```haskell
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
