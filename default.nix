{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101", biegunka  }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./dotfiles.nix { inherit biegunka; }
