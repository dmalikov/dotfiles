{ mkDerivation, acid-state, aeson, async, base, bytestring
, command-qq, conduit, conduit-extra, containers, cryptohash
, data-default-class, directory, directory-layout, exceptions
, fetchgit, filepath, free, hspec, hspec-expectations-lens
, HStringTemplate, lens, meep, mtl, optparse-applicative, process
, resourcet, safecopy, semigroups, stdenv, stm, template-haskell
, temporary, text, transformers, unix
}:
mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = fetchgit {
    url = "http://github.com/biegunka/biegunka.git";
    sha256 = "88641b4e44ed80fc4a44598554b6b654cb1f27e0852271d9a1dc3338bdf021ed";
    rev = "38dc810a39c0162aa06cb43cc4fa60f5a5d21019";
  };
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    acid-state aeson async base bytestring command-qq conduit
    conduit-extra containers cryptohash data-default-class directory
    directory-layout exceptions filepath free hspec HStringTemplate
    lens meep mtl optparse-applicative process resourcet safecopy
    semigroups stm template-haskell temporary text transformers unix
  ];
  testDepends = [
    base conduit conduit-extra containers data-default-class directory
    directory-layout filepath free hspec hspec-expectations-lens lens
    optparse-applicative process resourcet semigroups temporary text
    transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
