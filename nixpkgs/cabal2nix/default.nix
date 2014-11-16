{ haskellPackages ? (import <nixpkgs>).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "cabal2nix";
  version = "1.71";
  src = ../../git/cabal2nix/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ Cabal filepath hackageDb mtl regexPosix transformers ];
  testDepends = with haskellPackages; [ doctest ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/NixOS/cabal2nix";
    description = "Convert Cabal files into Nix build instructions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
