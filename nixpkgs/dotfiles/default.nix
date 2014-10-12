# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, biegunka, dataDefault, lens, optparseApplicative
, regexPcreBuiltin
}:

cabal.mkDerivation (self: {
  pname = "dotfiles";
  version = "9999";
  src = builtins.filterSource (path: type: builtins.baseNameOf path != ".hdevtools.sock") ((builtins.getEnv "HOME") + "/dmalikov/dotfiles/biegunka");
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    biegunka dataDefault lens optparseApplicative regexPcreBuiltin
  ];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
