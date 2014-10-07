# dotfiles [![Build Status](http://img.shields.io/travis/dmalikov/dotfiles/master.svg?style=flat-square)](http://travis-ci.org/dmalikov/dotfiles)

[`biegunka`][biegunka] scripts for keeping working environment configuration up-to-date

## tl;dr
- >1 environments
- configuration is stored in a template files, not in a ready-to-use format
- provisioning by [`biegunka`][biegunka]
- CI via Travis

## Why biegunka?
- regular `install.sh` is not enough when number of working environments is more than 1.
- regular `install.sh` is not enough when you need to remove all installed shit out of your system.
- regular `install.sh` is not enough when you need to rollback all changes.
- regular `install.sh` is not enough when you need some declarative syntax describing your configuration.
- regular `install.sh` is not enough actually because of `bash`.

Biegunka could do that for you.

## Another solutions

[`Chef`][chef] and [`chef-solo`][chef-solo] could be helpful here with all these recipes and roles power,
but I'm too lazy for writing a cookbook for every application that I have.

[`nix`][nix] language and package manager fits our needs perfectly here.
Installing nix over existed package manager and rewriting all dotfiles as a `nix` expressions can take much time.
But it is a perfect solution for a `nixos`.

## Usage

Particular working environment could be provisioned in a number of different ways:
- run biegunka executable with an updated version of [`Biegunka.hs`][biegunka-hs] (all necessary dependencies should be installed before that)
- compile [`dotfiles.cabal`][dotfiles-cabal] project pointed at [`Biegunka.hs`][biegunka-hs] and run builded `dotfiles` executable manually

### Nixos

```
$> nix-shell -p myHaskellPackages.dotfiles
nix-shell $> dotfiles --s10
```

[`myHaskellPackages.dotfiles` expression][dotfiles-nix] is defined in the local [`nixpkgs`][nixpkgs-config-nix].

### Gentoo

```
$> cabal install biegunka/Biegunka.hs
$> dotfiles --x220
```

TODO: use `biegunka` executable properly without breaking cabal package

### Windows
TODO: `biegunka` package depends on a `unix` package which is impossible to use inside a windows.

## Modularity
All dotfiles could be split into a number of modules. Each working environment is using only a subset of a all dotfiles stored in git repository.
that it is really needed.

E.g. `X`-less `nixos` environment doesn't need no `pentadactyl` or `mpv`

```haskell
profiles = sequence_
[ profile_git
, profile_haskell
, profile_nixpkgs
, profile_tmux
, profile_vifm
, profile_vim
, profile_zsh
]
```

which describes pretty well what is gonna be installed by running `biegunka`'s provisioning for this environment.

All profiles are defined in [`Profiles.hs`][profiles].  Almost all of them are using `dotfiles` function

```haskell
dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:dmalikov/dotfiles" "dmalikov/dotfiles" $ def & actions .~ as
```

Here is an example of `vim` profile:
```haskell
profile_vim :: Script Sources ()
profile_vim = do
  profile "vim/rc" $ do
    git_ "git@github.com:Shougo/neobundle.vim" ".vim/bundle/neobundle.vim"
    git "git@github.com:tpope/vim-pathogen" ".vim/bundle/vim-pathogen" $
      copy "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  profile "vim/syntax" $ do
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
    dotfiles $ copy "configs/vim/syntax/nix.vim" ".vim/after/syntax/nix.vim"
  profile "vim/colorschemes" $
    dotfiles $ copy "configs/vim/colors/neverland-darker.vim" ".vim/colors/neverland-darker.vim"
  profile "vim/plugins" $
    dotfiles $ copy "configs/vim/MyTabularMaps.vim" ".vim/bundle/tabular/after/plugin/MyTabularMaps.vim"
```

It's just a bunch of [script sources][biegunka-doc-script-sources] with a type `Script Sources ()`.

## Templating

Working with a number of different environments produce a lot of interesting problems. E.g. some parts
of a configuration files become dependant on a particular environment.

I.e. configuring tmux with a different default shell path.

tmux.conf:

```
set -g default-command $template.tmux.shell$
set -g default-shell $template.tmux.shell$
```

Tmux configuration in a [`Base.hs`][base-hs]:
```
data Tmux = Tmux
  { shell :: String
  } deriving (Data, Typeable)
```

Different environments has a different Tmux settings:

X220:
```haskell
settings :: Template
settings = def
  {
...
  , tmux = def
    { shell = "/bin/zsh"
    }
...
  }
```

S10:
```haskell
settings :: Template
settings = def
  {
...
  , tmux = def
    { shell = "~/.nix-profile/bin/zsh"
    }
...
  }
```

[biegunka-doc-script-sources]: http://biegunka.budueba.com/pages/script/actions.html
[biegunka-hs]:  https://github.com/dmalikov/dotfiles/blob/master/biegunka/Biegunka.hs
[biegunka]: https://github.com/biegunka
[chef-solo]: http://docs.opscode.com/chef_solo.html
[chef]: https://github.com/opscode/chef
[dotfiles-cabal]:  https://github.com/dmalikov/dotfiles/blob/master/biegunka/dotfiles.cabal
[dotfiles-nix]: https://github.com/dmalikov/dotfiles/blob/master/nixpkgs/dotfiles/default.nix
[nix]: http://nixos.org/nix/manual/
[nixpkgs-config-nix]: https://github.com/dmalikov/dotfiles/blob/master/nixpkgs/config.nix
[profiles]: https://github.com/dmalikov/dotfiles/blob/master/biegunka/Profiles.hs
[base-hs]: https://github.com/dmalikov/dotfiles/blob/master/biegunka/Environment/Base.hs
