{ mkDerivation, base, base64-bytestring, bytestring, e, h-gpgme
, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "e-gpgme";
  version = "0.1.0.0";
  src = (fetchgit {
    url = "http://github.com/dmalikov/e.git";
    sha256 = "194b9zr9f0qh00hijcq4121qqjx6y1qal59cr2pnsbyl8jm5f6a0";
    rev = "6a18760f31294f1eef391fe67fa5018985c48055";
  }) + "/e-gpgme/";
  libraryHaskellDepends = [
    base base64-bytestring bytestring e h-gpgme text
  ];
  description = "Cipher routine for e";
  license = stdenv.lib.licenses.bsd3;
}
