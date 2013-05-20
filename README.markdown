# dotfiles

Bunch of dotfiles managed by [biegunka](https://github.com/biegunka).

[![Build Status](https://drone.io/github.com/dmalikov/dotfiles/status.png)](https://drone.io/github.com/dmalikov/dotfiles/latest)

## What is it?

This is `biegunka`-powered repository containing my configuration files with
nice mechanism to maintain many work environments (there are only 2 of them
at now).

## Why biegunka?

Usual `install.sh` script sucks when you have more than 1 work station (which
is pretty common case in these days).

[`Chef`](https://github.com/opscode/chef) and
[`chef-folo`](http://docs.opscode.com/chef_solo.html) could be helpful here
with all these recipes and roles power, but I'm too lazy for writing a cookbook
for every application that I have.

`biegunka` could be used not just for dummy copying files to specific
directories, but for syncing git or darcs repository. It is usable when you
have many repositories and almost die of boredom updating them every day. It
could be successfully done by another `sh` script, but `biegunka` offers nice
readable declarative syntax and beatiful methods to formalize all that logic
nicely.

## How I'm using it?

When I'm using my `x220` workstation:

    $> cd biegunka
    $> cabal install
    $> dotfiles -e x220

If I want `biegunka` to show me what it gonna install and where:

    $> dotfiles --pretend -e x220

If I want `biegunka` to check that it create all files:

    $> dotfiles --verify -e x220

Also `--verify` and `--pretend` options could be used the same time.

When I'm using my `t510` workstation its all the same:

    $> dotfiles -e t510

## How it works?

`Main.hs` - main module of `dotfiles` cabal package. It contains command-line
options parser and `biegunka` function call:

    biegunka (set root "~") (pretend' <> execute (set templates $ Templates settings') <> verify') profiles'

`profiles` is a variable of `:: Script `Profiles ()` type. It contains all
information about what _profiles_ will be installed.

`Profiles.hs` file contains all profiles that I have. Here is example of xmonad
profile:

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

`dotfiles` is a function stands for a link to my dotfile repository:

    dotfiles :: Script Actions () -> Script Sources ()
    dotfiles as = git' "git@github.com:dmalikov/dotfiles" "projects/dmalikov/dotfiles" $ def & actions .~ as

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

    $if(template.git.set_user)$
    [user]
        name = $template.git.user_name$
        email = $template.git.user_email$
    $endif$

`X220` modules contains definition for these template placeholders:

    settings :: Template
    settings = def
      { git = def
        { set_user = True
        , user_name = "Dmitry Malikov"
        , user_email = "malikov.d.y@gmail.com"
        }
    ...

`T510` environment doesn't have these variables and `set_user` is equal `False`
for it.

`git` profiles has these call:

    substitute "configs/git/config.template" ".gitconfig"

So it produces `[user]` part of gitconfig only for `X220` environment:

    [user]
        name = Dmitry Malikov
        email = malikov.d.y@gmail.com



