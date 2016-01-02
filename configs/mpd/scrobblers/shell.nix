{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7102"
, liblastfmPath
, scrobblersPath
}:
let
  inherit (nixpkgs) pkgs;
  h = pkgs.haskell.packages.${compiler};
  l = h.callPackage liblastfmPath {
    mkDerivation = args: h.mkDerivation(args // {
      doCheck = false;
      doHaddock = false;
    });
  };
  s = h.callPackage scrobblersPath {
    mkDerivation = args: h.mkDerivation(args // {
      doCheck = false;
      doHaddock = false;
    });
    liblastfm = l;
  };
  ghc = h.ghcWithPackages (ps: with ps; [ l s ]);
in
pkgs.stdenv.mkDerivation {
  name = "run-scrobblers";
  buildInputs = [ ghc ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
    runhaskell ./Main.hs
  '';
}
