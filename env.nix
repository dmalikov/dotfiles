let
  pkgs = import <nixpkgs> {};
  env = pkgs.haskellngPackages.ghcWithPackages (self: ( with self; [
    hlint hdevtools doctest stylish-haskell
    ] ++ (self.callPackage ./. { pkgs = pkgs ; }).nativeBuildInputs));
in
  pkgs.myEnvFun {
    name = "dotfiles";
    shell = "zsh";
    buildInputs = [ pkgs.haskellngPackages.cabal-install env ];
    extraCmds = ''
      $(grep export ${env.outPath}/bin/ghc)
    '';
    }
