{ mkDerivation, acid-state, aeson, async, base, bytestring
, command-qq, conduit, conduit-extra, containers, cryptohash
, data-default-class, directory, directory-layout, exceptions
, filepath, free, hspec, hspec-expectations-lens, HStringTemplate
, lens, meep, mtl, optparse-applicative, process, resourcet
, safecopy, semigroups, stdenv, stm, template-haskell, temporary
, text, transformers, unix, fetchgit, biegunkaSrc ? "remote"
}:
mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = if biegunkaSrc == "remote" then fetchgit {
    url = "https://github.com/biegunka/biegunka.git";
    sha256 = "38f00404ead627190561e9c6576f80f57b4fc7894b98be3dd0c1572a003b88cc";
    rev = "332376ccddba1ad12b33d7900d9c3fa4caa420a5";
    fetchSubmodules = false;
  } else ../biegunka/.;
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
    base containers data-default-class directory directory-layout
    filepath free hspec hspec-expectations-lens lens
    optparse-applicative semigroups temporary text transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
  doCheck = false;
}
