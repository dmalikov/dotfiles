{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "hspec-contrib";
  version = "0.2.0";
  sha256 = "0p6jh3j84cq66gfp2pk957ha4ds7797vfbfri9lsg9gd4998dwkq";
  buildDepends = with haskellPackages; [ hspecCore HUnit ];
  testDepends = with haskellPackages; [ hspec hspecCore HUnit QuickCheck ];
  meta = {
    homepage = "http://hspec.github.io/";
    description = "Contributed functionality for Hspec";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
