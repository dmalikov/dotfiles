with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        biegunka = self.callPackage ./biegunka.nix {};
        dotfiles = self.callPackage ./default.nix {};
      };
    };
in modifiedHaskellPackages.dotfiles.env
