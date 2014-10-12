# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, cereal, cipherAes128, cryptoApi, dataDefaultClass
, exceptions, hspec, httpConduit, httpTypes, lens, lensAeson
, liblastfm, libmpd, mtl, netwire, network, QuickCheck, semigroups
, text, time
}:

cabal.mkDerivation (self: {
  pname = "scrobblers";
  version = "0.1.0.0";
  src = /home/yep/git/scrobblers;
  buildDepends = [
    aeson cereal cipherAes128 cryptoApi dataDefaultClass exceptions
    httpConduit httpTypes lens lensAeson liblastfm libmpd mtl netwire
    network semigroups text time
  ];
  testDepends = [
    cipherAes128 cryptoApi dataDefaultClass hspec lens mtl netwire
    network QuickCheck text
  ];
  meta = {
    description = "Lastfm scrobblers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})