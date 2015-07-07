{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101", biegunka ? ./biegunka.nix }: let
  inherit (nixpkgs) pkgs;
  h = pkgs.haskell.packages.${compiler};
  biegunka- = h.callPackage biegunka { mkDerivation = args: h.mkDerivation ( args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ nixpkgs.pkgs.git ];
      doCheck = false;
        # dyld: Library not loaded: /nix/store/z2d0y81mb7bx9gkjcmhsal2w8nqjl9a1-libiconv-1.14/lib/libiconv.2.dylib
        #          Referenced from: /nix/store/d9cjg503si90fyd4vgjan9qsp58jm6cf-git-2.4.4/bin/git
        #          Reason: Incompatible library version: git requires version 8.0.0 or later, but libiconv.2.dylib provides version 7.0.0
      doHaddock = false;
    });
  };
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
