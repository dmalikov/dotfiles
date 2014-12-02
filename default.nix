{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "dotfiles";
  version = "9999";
  src = builtins.filterSource (_: type: type != "unknown") ./biegunka/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [
    biegunka dataDefault lens optparseApplicative regexPcreBuiltin
  ];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
