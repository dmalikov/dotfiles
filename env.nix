let
  pkgs = import <nixpkgs> {};
  env = pkgs.haskellPackages.ghcWithPackagesOld (self: ( with self; [
    hlint
    hdevtools
    doctest ] ++
    (self.callPackage ./. { pkgs = pkgs ; }).nativeBuildInputs));
in
  pkgs.myEnvFun {
    name = "dotfiles";
    shell = "zsh";
    buildInputs = [ pkgs.haskellPackages.cabalInstall env ];
    extraCmds = ''
      $(grep export ${env.outPath}/bin/ghc)
    '';
    }
