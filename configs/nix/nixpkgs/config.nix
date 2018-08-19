{ pkgs }: {

  packageOverrides = super: let pkgs = super.pkgs; in with pkgs; rec {

    hs-env = pkgs.buildEnv {
      name = "hs-env";
      paths = with pkgs.haskellPackages; [ stack hlint ghcid fast-tags hspec-discover hpack weeder ];
    };

  };
}
