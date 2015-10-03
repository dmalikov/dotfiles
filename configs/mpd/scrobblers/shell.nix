{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
let
  inherit (nixpkgs) pkgs;
  s = pkgs.haskell.packages.${compiler}.callPackage ./scrobblers.nix {};
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [ s ]);
in
pkgs.stdenv.mkDerivation {
  name = "run-scrobblers";
  buildInputs = [ ghc ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
    runhaskell ./Main.hs
  '';
}
