# dotfiles

Bunch of dotfiles managed by [biegunka](https://github.com/biegunka).

Drone.io status: [![Build Status](https://drone.io/github.com/dmalikov/dotfiles/status.png)](https://drone.io/github.com/dmalikov/dotfiles/latest)

## What is it?

This is biegunka-powered repository containing my configuration files with nice
mechanism to maintain many work environments (there are only two of them at
now).

## Why biegunka?

Usual `install.sh` script sucks when you have more than 1 work station (which
is pretty common case in these days).

`Chef` and `chef-folo` could be helpful with all these recipes and roles power,
but I'm too lazy for write a cookbook for every application that I have.

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

When I'm using my `t510` workstation its all the same

    $> dotfiles -e t510

