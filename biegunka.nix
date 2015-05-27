{ mkDerivation, acid-state, aeson, ansi-wl-pprint, async, base
, bytestring, command-qq, containers, data-default-class, directory
, directory-layout, exceptions, filepath, free, hspec
, hspec-expectations-lens, HStringTemplate, lens, meep, mtl
, optparse-applicative, pointed, process, reflection, safecopy, semigroupoids, semigroups
, stdenv, stm, template-haskell, temporary, terminal-size, tagged, tagged-transformer,  text
, transformers, unix, void, fetchgit
}:
mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/biegunka/biegunka.git";
    sha256 = "7bef293063dfb6bb89fd37fac9533267a66f764e42fcdaf3face92f0aee4f160";
    rev = "adf703ce6ecb6197c4838ad1a7ae5e25a044c7d7";
    fetchSubmodules = false;
  };
  isLibrary = true;
  isExecutable = false;
  buildDepends = [
    acid-state aeson ansi-wl-pprint async command-qq data-default-class
    directory-layout exceptions filepath free hspec HStringTemplate lens
    meep mtl optparse-applicative pointed reflection safecopy semigroups
    semigroupoids
    stm tagged tagged-transformer terminal-size text transformers void
  ];
  testDepends = [
    data-default-class directory-layout filepath free hspec
    hspec-expectations-lens lens optparse-applicative semigroups temporary
    text transformers
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
  doCheck = false; # https://github.com/biegunka/biegunka/issues/62
}
