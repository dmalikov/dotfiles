# dotfiles

Bunch of dotfiles managed by [biegunka](https://github.com/biegunka).

Drone.io status: [![Build Status](https://drone.io/github.com/dmalikov/dotfiles/status.png)](https://drone.io/github.com/dmalikov/dotfiles/latest)

## What is it?

This is `biegunka`-powered repository containing my configuration files with
nice mechanism to maintain many work environments (there are only 2 of them
at now).

## Why biegunka?

Usual `install.sh` script sucks when you have more than 1 work station (which
is pretty common case in these days).

[`Chef`](https://github.com/opscode/chef) and
[`chef-folo`](http://docs.opscode.com/chef_solo.html) could be helpful with all
these recipes and roles power, but I'm too lazy for write a cookbook for every
application that I have.

Beside of simple copying files to directory `biegunka` could be used for
syncing git or cabal repository. Its quite usable when you have many
repositories and almost die of boredom updating them every day. It could be
successfully done by another `sh` script, but `biegunka` offers nice readable
declarative syntax.

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

