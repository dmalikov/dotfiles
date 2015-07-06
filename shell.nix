{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101", biegunka ? ./biegunka.nix }: let
  inherit (nixpkgs) pkgs;
  h = pkgs.haskell.packages.${compiler};
  biegunka- = pkgs.stdenv.lib.overrideDerivation (h.callPackage biegunka {}) (_ : {
    doCheck = false;
    doHaddock = false;
  });
  ghc = h.ghcWithPackages (ps: with ps; [ biegunka- cabal2nix cabal-install data-default hdevtools stylish-haskell ]);
in
  pkgs.stdenv.mkDerivation rec {
    name = "dotfiles";
    buildInputs = [ ghc ];
    shellHook = ''
      eval $(egrep ^export ${ghc}/bin/ghc)
      export IN_WHICH_NIX_SHELL=${name}
    '';
  }
