{ pkgs ? (import <nixpkgs> {}) }:

pkgs.haskellngPackages.mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = pkgs.fetchgit {
    url = "https://github.com/biegunka/biegunka.git";
    sha256 = "7bef293063dfb6bb89fd37fac9533267a66f764e42fcdaf3face92f0aee4f160";
    rev = "adf703ce6ecb6197c4838ad1a7ae5e25a044c7d7";
    fetchSubmodules = false;
  };
  isLibrary = true;
  isExecutable = false;
  buildDepends = with pkgs.haskellngPackages; [
    acid-state aeson ansi-wl-pprint async command-qq data-default-class
    directory-layout exceptions filepath free hspec HStringTemplate lens
    meep mtl optparse-applicative pointed reflection safecopy semigroups
    stm tagged tagged-transformer terminal-size text transformers void
  ];
  testDepends = with pkgs.haskellngPackages; [
    data-default-class directory-layout filepath free hspec
    hspec-expectations-lens lens optparseApplicative semigroups temporary
    text transformers
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = pkgs.stdenv.lib.licenses.mit;
  doCheck = false; # https://github.com/biegunka/biegunka/issues/62
}
