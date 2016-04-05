{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7103"
}: let
  inherit (nixpkgs) pkgs;
  h = pkgs.haskell.packages.${compiler};
  hdevtools = h.callPackage ./hdevtools.nix {};
  e = h.callPackage ../e/e.nix {
    mkDerivation = args: h.mkDerivation(args // {
      doCheck = false;
      doHaddock = false;
    });
  };
  biegunka = h.callPackage ../biegunka/biegunka.nix {
    mkDerivation = args: h.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ pkgs.git ];
      doCheck = false;
      doHaddock = false;
    });
    e = e;
    meep = h.callPackage ../meep/package.nix { };
  };
  ghc = h.ghcWithPackages (ps: [ ps.data-default biegunka e ]);
in
  pkgs.stdenv.mkDerivation rec {
    name = "dotfiles";
    buildInputs = with h; [ biegunka cabal-install cabal2nix hdevtools e ghc ];
    shellHook = ''
      eval $(egrep ^export ${ghc}/bin/ghc)
    '';
  }
