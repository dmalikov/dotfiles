{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  inherit (nixpkgs) pkgs;
  biegunka = pkgs.haskell.packages.${compiler}.callPackage ./biegunka.nix {};
  cabal-install = pkgs.haskell.packages.${compiler}.cabal-install;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [ biegunka hdevtools stylish-haskell ]);
  pkg = (import ./default.nix { inherit nixpkgs compiler biegunka; });
in
  pkgs.stdenv.mkDerivation rec {
    name = pkg.pname;
    buildInputs = [ ghc cabal-install ] ++ pkg.env.buildInputs;
    shellHook = ''
      ${pkg.env.shellHook}
      export IN_WHICH_NIX_SHELL=${name}
      cabal configure --package-db=$NIX_GHC_LIBDIR/package.conf.d
    '';
  }
