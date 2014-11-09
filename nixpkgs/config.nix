pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: _: {
          biegunka = self.callPackage ../git/biegunka { haskellPackages = myHaskellPackages; };
          cabal2nix = self.callPackage ../.nixpkgs/cabal2nix {};
          dotfiles = self.callPackage ../git/dotfiles { haskellPackages = myHaskellPackages; };
          hstorrent = self.callPackage ../git/hstorrent {};
          liblastfm = self.callPackage ../git/liblastfm { haskellPackages = myHaskellPackages; };
          scrobblers = self.callPackage ../git/scrobblers { haskellPackages = myHaskellPackages; };
        };
      });
  };
}
