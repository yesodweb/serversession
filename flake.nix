{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs =
    inputs@{
      nixpkgs,
      flake-parts,
      haskellNix,
      ...
    }:
    let
      stackFileset =
        lib:
        lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            # dir
            ./examples/serversession-example-yesod-persistent
            ./serversession
            ./serversession-backend-acid-state
            ./serversession-backend-persistent
            ./serversession-backend-redis
            ./serversession-frontend-wai
            ./serversession-frontend-yesod
            # file
            ./stack.yaml
            ./stack.yaml.lock
          ];
        };
      overlays = [
        haskellNix.overlay
        (final: prev: {
          project = final.haskell-nix.stackProject' {
            src = stackFileset final.lib;
            # When haskell.nix rebuilds `unix`, `directory`, or `process` on GHC 9.10+,
            # it passes `-os-string` by default,
            # leaving `os-string` as a hidden package and breaking imports.
            # These packages require `+os-string` to work with the newer `filepath`,
            # so enable the flag explicitly.
            # https://github.com/input-output-hk/haskell.nix/issues/2423
            modules = [
              {
                packages.directory.flags.os-string = true;
                packages.process.flags.os-string = true;
                packages.unix.flags.os-string = true;
              }
            ];
            shell = {
              tools = {
                haskell-language-server = "latest";
              };
              buildInputs = with prev; [
                stack
                zlib

                (writeScriptBin "haskell-language-server-wrapper" ''
                  #!${stdenv.shell}
                  exec haskell-language-server "$@"
                '')
              ];
            };
          };
        })
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
      ];

      perSystem =
        { system, ... }:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          flake = pkgs.project.flake { };
        in
        {
          checks = flake.packages // flake.checks;

          inherit (flake)
            apps
            devShells
            packages
            ;
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org/"
      "https://cache.iog.io/"
      "https://ncaq.cachix.org/"
      "https://niks3-public.ncaq.net/"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "ncaq.cachix.org-1:XF346GXI2n77SB5Yzqwhdfo7r0nFcZBaHsiiMOEljiE="
      "niks3-public.ncaq.net-1:e/B9GomqDchMBmx3IW/TMQDF8sjUCQzEofKhpehXl04="
    ];
    allow-import-from-derivation = true; # required by haskell.nix
  };
}
