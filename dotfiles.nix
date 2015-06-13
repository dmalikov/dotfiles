{ mkDerivation, base, biegunka, data-default, lens
, optparse-applicative, regex-pcre-builtin, stdenv
}:
mkDerivation {
  pname = "dotfiles";
  version = "9999";
  src = builtins.filterSource (_: type: type != "unknown") ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base biegunka data-default lens optparse-applicative
    regex-pcre-builtin
  ];
  license = stdenv.lib.licenses.mit;
}
