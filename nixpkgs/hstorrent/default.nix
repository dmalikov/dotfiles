{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: rec {
  pname = "hstorrent";
  version = "0.1.0.0";
  src = ../../git/hstorrent/.;
  buildDepends = with haskellPackages; [
    bencoding binary dataDefault lens QuickCheck quickcheckInstances
  ];
  testDepends = with haskellPackages; buildDepends ++ [
    filepath hspec hspecContrib HUnit QuickCheck quickcheckInstances
  ];
  meta = {
    homepage = "https://github.com/hstorrent/hstorrent";
    description = "BitTorrent library in Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
