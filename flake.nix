# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Playground to run haskell code in Telegram";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "playground-hs";

        genericPackages = pkgs: with pkgs;
          [ lens
            wreq
            servant-server
            servant-client
            typed-process
          ];
        mkdockerImage = additionalPackages: ghcVer:
          pkgs.dockerTools.buildLayeredImage {
            name = ghcVer + "env";
            contents = [
              (pkgs.haskell.packages.${ghcVer}.ghcWithPackages
                (pkgs: genericPackages pkgs ++ additionalPackages pkgs))
              pkgs.bash
              pkgs.coreutils
            ];
        };
        mkSimpleDockerImage = mkdockerImage ( _ : []);
        envVars = workers: with pkgs; {
          GHC902_IMAGE = mkSimpleDockerImage "ghc902";
          SCRIPTS_DIR = ./scripts;
          WORKERS_COUNT = toString workers;
          DOCKER = "${docker}/bin/docker";
        };
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            resource-pool = haskellPackages.callCabal2nix "resource-pool" (
              builtins.fetchGit {
                url = "https://github.com/qnikst/pool.git";
                rev = "5b04d120057ba5ff3c00f7e41e6c5c1a1a3cc0fe";
              }
            ){};
          };

        defaultPackage = self.packages.${system}.${packageName};

        nixosModules.default = with pkgs.lib; { config, ... }:
          let cfg = config.services.playground-hs;
          in
          {
            options.services.playground-hs = {
              enable = mkEnableOption "playground-hs bot for Telegram";
              envFile = mkOption {
                type = types.str;
                default = "/etc/playground-hs.env";
              };
              workersCount = mkOption {
                type = types.int;
                default = 8;
              };
            };
            config = mkIf cfg.enable {
              users.users.playground-hs = {
                group = "docker";
                isSystemUser = true;
              };

              virtualisation.docker.enable = true;

              systemd.services.playground-hs = {
                wantedBy = [ "multi-user.target" ];
                serviceConfig = {
                    ExecStart = "${self.defaultPackage.${system}}/bin/playground-hs";
                    EnvironmentFile = cfg.envFile;
                    Environment = concatStringsSep " " (pkgs.lib.mapAttrsToList (name: value: name + "=" + value) (envVars cfg.workersCount));
                    User = "playground-hs";
                    Group = "docker";
                };
              };
            };
          };
        devShell = pkgs.mkShell ({
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghc
            cabal-install
            lzma
            zlib
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        } // envVars 8);
      });
}
