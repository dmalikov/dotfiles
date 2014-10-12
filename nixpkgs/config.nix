with import <nixpkgs> {};

pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: super: {
          biegunka = callPackage ../.nixpkgs/biegunka {};
          cabal2nix = callPackage ../.nixpkgs/cabal2nix {};
          dotfiles = callPackage ../.nixpkgs/dotfiles {};
          liblastfm = callPackage ../.nixpkgs/liblastfm {};
          lensAeson = callPackage ../.nixpkgs/lens-aeson {};
          scrobblers = callPackage ../.nixpkgs/scrobblers {};
        };
      });
  };
}

