{ mkDerivation, aeson, base, bytestring, cereal, containers
, exceptions, hspec, http-client, http-types, lens, lens-aeson
, liblastfm, libmpd, mtl, netwire, network, QuickCheck, semigroups
, stdenv, text, time, fetchgit
}:
mkDerivation {
  pname = "scrobblers";
  version = "0.1.0.0";
  src = builtins.filterSource (_: type: type != "unknown") ~/git/scrobblers;
  buildDepends = [
    aeson base bytestring cereal containers exceptions http-client
    http-types lens lens-aeson liblastfm libmpd mtl netwire network
    semigroups text time
  ];
  testDepends = [
    base bytestring hspec lens netwire network QuickCheck
  ];
  description = "Lastfm scrobblers";
  license = stdenv.lib.licenses.bsd3;
}
