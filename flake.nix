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
        inherit (pkgs) lib;

        haskellPackages = pkgs.haskell.packages.ghc912;
        buildDay =
          name:
          pkgs.runCommand name
            {
              src = ./. + ("/" + name + ".hs");
              nativeBuildInputs = [ haskellPackages.ghc ];
            }
            ''
              mkdir -p $out/bin
              ghc -O3 $src -o $out/bin/$name
            '';

        days = lib.pipe (builtins.readDir ./.) [
          (lib.filterAttrs (n: v: v == "regular" && lib.hasSuffix ".hs" n))
          builtins.attrNames
          (builtins.map (lib.removeSuffix ".hs"))
          (lib.flip lib.genAttrs buildDay)
        ];

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

        packages = days // {
          all = pkgs.symlinkJoin {
            name = "all";
            paths = builtins.attrValues days;
          };
          default = self.packages.${system}.all;
        };
      }
    );
}
