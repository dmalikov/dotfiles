{ compiler }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              dotfiles = haskellPackagesNew.callPackage ./default.nix { };
              e = haskellPackagesNew.callPackage ./e.nix { };
              e-gpgme = haskellPackagesNew.callPackage ./e-gpgme.nix { };
              biegunka = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (haskellPackagesNew.callPackage ./biegunka.nix { }));
              meep = haskellPackagesNew.callPackage ./meep.nix { };
              h-gpgme = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak haskellPackagesOld.h-gpgme);
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskell.packages.${compiler}.dotfiles;
  }
