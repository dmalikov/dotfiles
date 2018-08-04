{ mkDerivation, aeson, aeson-pretty, attoparsec, base, binary
, bytestring, criterion, deepseq, deepseq-generics, directory
, either, filepath, hashable, hspec, lens, mtl, parsers, QuickCheck
, quickcheck-instances, stdenv, temporary, text, transformers
, unordered-containers, fetchgit
}:
mkDerivation {
  pname = "e";
  version = "0.1.0.0";
  src = (fetchgit {
    url = "http://github.com/dmalikov/e.git";
    sha256 = "194b9zr9f0qh00hijcq4121qqjx6y1qal59cr2pnsbyl8jm5f6a0";
    rev = "6a18760f31294f1eef391fe67fa5018985c48055";
  }) + "/e/";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base bytestring directory either
    hashable lens mtl parsers text transformers unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base binary directory filepath hspec QuickCheck
    quickcheck-instances temporary text unordered-containers
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq deepseq-generics
  ];
  description = "Express sensitive data inside text files in readable and compact way";
  license = stdenv.lib.licenses.bsd3;
}
