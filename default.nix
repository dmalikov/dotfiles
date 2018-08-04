{ mkDerivation, base, biegunka, data-default, lens, stdenv }:
mkDerivation {
  pname = "dotfiles";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base biegunka data-default lens ];
  license = stdenv.lib.licenses.bsd3;
}
