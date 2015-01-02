{ pkgs ? (import <nixpkgs> {}) }:

pkgs.haskellPackages.cabal.mkDerivation (self: {
  pname = "biegunka";
  version = "0.2";
  src = pkgs.fetchgit {
    url = "https://github.com/biegunka/biegunka.git";
    sha256 = "3c3216314d4f798647f9e56d439e31119093fe76a6cad5a132c6478b15307c15";
    rev = "b5adfa6dcb2fe579f3d78bb1e51ce3889ec9daad";
    fetchSubmodules = false;
  };
  isLibrary = true;
  isExecutable = false;
  buildDepends = with pkgs.haskellPackages; [
    acidState aeson ansiWlPprint async commandQq dataDefaultClass
    directoryLayout exceptions filepath free hspec HStringTemplate lens
    meep mtl optparseApplicative pointed reflection safecopy semigroups
    stm tagged taggedTransformer terminalSize text transformers void
  ];
  testDepends = with pkgs.haskellPackages; [
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
