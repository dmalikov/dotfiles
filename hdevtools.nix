{ mkDerivation, base, cmdargs, directory, fetchgit, filepath, ghc
, ghc-paths, network, stdenv, syb, time, unix
}:
mkDerivation {
  pname = "hdevtools";
  version = "0.1.0.7";
  src = fetchgit {
    url = "https://github.com/supki/hdevtools";
    sha256 = "412379f2425b822f7ba25aabd8ab236e22464a691bd8b2dc2f31ae8bac9d1f1e";
    rev = "4fb6409a22dcb6607d495c81cad0d117bc11fdf9";
  };
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base cmdargs directory filepath ghc ghc-paths network syb time unix
  ];
  homepage = "https://github.com/bitc/hdevtools/";
  description = "Persistent GHC powered background server for FAST haskell development tools";
  license = stdenv.lib.licenses.mit;
}
