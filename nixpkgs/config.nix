pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: _: {
          cabal2nix        = self.callPackage ../.nixpkgs/cabal2nix    { haskellPackages = myHaskellPackages; };
          liblastfm        = self.callPackage ../git/liblastfm         { haskellPackages = myHaskellPackages; };
          scrobblers       = self.callPackage ../git/scrobblers        { haskellPackages = myHaskellPackages; };
        };
      });
  };
}
