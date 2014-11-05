with import <nixpkgs> {};

pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: super: {
          biegunka = self.callPackage ../.nixpkgs/biegunka {};
          cabal2nix = self.callPackage ../.nixpkgs/cabal2nix {};
          dotfiles = self.callPackage ../.nixpkgs/dotfiles {};
          hstorrent = self.callPackage ../.nixpkgs/hstorrent {};
          liblastfm = self.callPackage ../.nixpkgs/liblastfm {};
          scrobblers = self.callPackage ../.nixpkgs/scrobblers {};
        };
      });
  };
}

