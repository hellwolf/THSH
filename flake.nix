{
  description = "Nix flake for local development and CI of the THSH project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    (flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        commonDevInputs = with pkgs; [
          jq
          shellcheck
        ];
        commonShellHook = ''
          # This makes binaries of this project available for testing, e.g. `yolc`
          export PATH=$PWD/bin/:$PATH
        '';
        mkCI =
          ghcVer:
          pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal-install
              hlint
              haskell.compiler.${ghcVer}
            ];
            shellHook = commonShellHook;
          };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            commonDevInputs
            ++ [
              # haskell local dev tooling
              cabal-install
              hlint
              stylish-haskell
              haskell.compiler.ghc910
              haskell.packages.ghc910.haskell-language-server
            ];
          shellHook = commonShellHook;
        };

        devShells.ci-ghc92  = mkCI "ghc92";
        devShells.ci-ghc94  = mkCI "ghc94";
        devShells.ci-ghc96  = mkCI "ghc96";
        devShells.ci-ghc98  = mkCI "ghc98";
        devShells.ci-ghc910 = mkCI "ghc910";
      }
    ));
}
