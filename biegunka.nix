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
    sha256 = "26b496498a4047c46c6cc8d8fe8762c42beede8e2c3f06f76b9087e7d37f3034";
    rev = "d46624126b6edd08aa87cb5b9cad0b262e901a1a";
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
