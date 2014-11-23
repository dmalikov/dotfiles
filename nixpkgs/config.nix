pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: _: {
          biegunka         = self.callPackage ../git/biegunka          { haskellPackages = myHaskellPackages; };
          cabal2nix        = self.callPackage ../.nixpkgs/cabal2nix    { haskellPackages = myHaskellPackages; };
          dotfiles         = self.callPackage ../git/dotfiles          { haskellPackages = myHaskellPackages; };
          hspecContrib     = self.callPackage ../.nixpkgs/hspecContrib { haskellPackages = myHaskellPackages; };
          hstorrent        = self.callPackage ../.nixpkgs/hstorrent    { haskellPackages = myHaskellPackages; };
          liblastfm        = self.callPackage ../git/liblastfm         { haskellPackages = myHaskellPackages; };
          liblastfmExample = self.callPackage ../git/liblastfm/example { haskellPackages = myHaskellPackages; };
          scrobblers       = self.callPackage ../git/scrobblers        { haskellPackages = myHaskellPackages; };
        };
      });
  };
}
