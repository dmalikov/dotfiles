{ mkDerivation, base, directory, filepath, hspec, stdenv, text
, text-metrics
}:
mkDerivation {
  pname = "infer-license";
  version = "0.2.0";
  sha256 = "12e6fe616575159c03cf6fd4a4f30021ecf264a529ab4e4edd6e96e296a98e72";
  libraryHaskellDepends = [
    base directory filepath text text-metrics
  ];
  testHaskellDepends = [
    base directory filepath hspec text text-metrics
  ];
  description = "Infer software license from a given license file";
  license = stdenv.lib.licenses.mit;
}
