{ pkgs ? (import <nixpkgs> {}) }:

let biegunka = import ./biegunka.nix {}; in

pkgs.haskellngPackages.mkDerivation {
  pname = "dotfiles";
  version = "9999";
  src = builtins.filterSource (_: type: type != "unknown") ./biegunka/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with pkgs.haskellngPackages; [
    biegunka data-default lens optparse-applicative regex-pcre-builtin
  ];
  license = pkgs.stdenv.lib.licenses.mit;
}
