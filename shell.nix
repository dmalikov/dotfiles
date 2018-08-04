let
  pkgs = import <nixpkgs> { };
in
  (import ./release.nix { compiler = "ghc843"; }).project.env.overrideAttrs (oldAttrs: { buildInputs = oldAttrs.buildInputs ++ (with pkgs.haskellPackages; [ ]); })
