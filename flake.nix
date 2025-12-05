{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:

    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc912;
      in
      {
        devShell = haskellPackages.shellFor {
          packages = p: with haskellPackages; [ ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            fourmolu
            ghcid
          ];
        };
      }
    );
}
