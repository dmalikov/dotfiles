{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "biegunka";
  version = "0.2";
  src = fetchgit {
    url = "git@github.com:biegunka/biegunka.git";
    sha256 = "545e6e86f229e6f5c53faa581a607c4be14c81948c8d6ab861881edceee66e59";
    rev = "e61187c74a089411d2b23b5234a7cd93de5d8c92";
    fetchSubmodules = false;
  };
  isLibrary = true;
  isExecutable = false;
  buildDepends = with haskellPackages; [
    acidState aeson ansiWlPprint async commandQq dataDefaultClass
    directoryLayout exceptions filepath free hspec HStringTemplate lens
    meep mtl optparseApplicative pointed reflection safecopy semigroups
    stm tagged taggedTransformer terminalSize text transformers void
  ];
  testDepends = with haskellPackages; [
    dataDefaultClass directoryLayout filepath free hspec
    hspecExpectationsLens lens optparseApplicative semigroups temporary
    text transformers
  ];
  meta = {
    homepage = "http://biegunka.budueba.com/";
    description = "Configuration development";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
  doCheck = false; # https://github.com/biegunka/biegunka/issues/62
})
