{ pkgs }: {

  packageOverrides = super: let pkgs = super.pkgs; in with pkgs; rec {

    infer-license = pkgs.haskellPackages.callPackage ./infer-license.nix { };
    hpack = pkgs.haskellPackages.callPackage ./hpack.nix { infer-license=infer-license; };

    hs-env = pkgs.buildEnv {
      name = "hs-env";
      paths = with pkgs.haskellPackages; [ stack hlint ghcid fast-tags hspec-discover hpack weeder ];
    };

  };
}
