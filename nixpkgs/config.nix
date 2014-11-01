with import <nixpkgs> {};

pkgs: {
  packageOverrides = pkgs : rec {
    myHaskellPackages =
      let callPackage = pkgs.lib.callPackageWith myHaskellPackages; in
      pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self: super: {
          asn1Encoding = pkgs.stdenv.lib.overrideDerivation super.asn1Encoding (oldAttrs : { doCheck = false; } );
          biegunka = self.callPackage ../.nixpkgs/biegunka {};
          cabal2nix = self.callPackage ../.nixpkgs/cabal2nix {};
          dotfiles = self.callPackage ../.nixpkgs/dotfiles {};
          hstorrent = self.callPackage ../.nixpkgs/hstorrent {};
          liblastfm = self.callPackage ../.nixpkgs/liblastfm {};
          lensAeson = self.callPackage ../.nixpkgs/lens-aeson {};
          scrobblers = self.callPackage ../.nixpkgs/scrobblers {};
        };
      });
  };
}

