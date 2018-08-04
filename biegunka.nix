{ mkDerivation, acid-state, aeson, async, base, bytestring, Cabal
, command-qq, conduit, conduit-extra, containers, cryptonite
, data-default-class, directory, directory-layout, e, e-gpgme
, exceptions, filepath, free, hspec, hspec-expectations-lens
, HStringTemplate, lens, meep, mtl, optparse-applicative, process
, resourcet, safecopy, semigroups, stdenv, stm, template-haskell
, temporary, text, transformers, unix, fetchgit
}:
mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = fetchgit {
    url = "http://github.com/biegunka/biegunka.git";
    sha256 = "1dkpyzfimfjdn06d5i428llkzskjvad6inag4r82dxm0ws2ffjvd";
    rev = "74fc93326d5f29761125d7047d5418899fa2469d";
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [
    acid-state async base bytestring command-qq conduit conduit-extra
    containers cryptonite directory directory-layout e e-gpgme
    exceptions filepath free hspec HStringTemplate lens meep mtl
    optparse-applicative process resourcet safecopy semigroups stm
    template-haskell temporary text transformers unix
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra containers
    data-default-class directory filepath lens process resourcet text
    transformers unix
  ];
  testHaskellDepends = [
    base conduit conduit-extra containers data-default-class directory
    directory-layout filepath free hspec hspec-expectations-lens lens
    optparse-applicative process resourcet semigroups temporary text
    transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
