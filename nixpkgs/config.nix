with import <nixpkgs> {};

pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: super: {
          biegunka = callPackage /home/yep/.nixpkgs/biegunka {};
          cabal2nix = callPackage /home/yep/.nixpkgs/cabal2nix {};
          dotfiles = callPackage /home/yep/.nixpkgs/dotfiles {};
          liblastfm = callPackage /home/yep/.nixpkgs/liblastfm {};
          lensAeson = callPackage /home/yep/.nixpkgs/lens-aeson {};
          scrobblers = callPackage /home/yep/.nixpkgs/scrobblers {};
        };
      });
  };
}

