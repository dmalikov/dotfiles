with import <nixpkgs> {};

pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: super: {
          biegunka = self.callPackage ../git/biegunka {};
          cabal2nix = self.callPackage ../.nixpkgs/cabal2nix {};
          dotfiles = self.callPackage ../git/dotfiles {};
          hstorrent = self.callPackage ../git/hstorrent {};
          liblastfm = self.callPackage ../git/liblastfm {};
          scrobblers = self.callPackage ../git/scrobblers {};
        };
      });
  };
}

