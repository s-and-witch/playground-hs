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
        globalPkgs = pkgs;

        overrideHaskellPackages = hp:
          hp.override { overrides = self: super: { mkDerivation = args: super.mkDerivation (args // { doCheck = false; doHaddock = false; }); }; };

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "playground-hs";

        genericPackages = pkgs: with pkgs;
          [ lens
            effectful
            conduit
            streaming
            aeson
            lens-aeson
            megaparsec
            typed-process
          ];

        mkGhcFrom = hp: hp.ghcWithPackages genericPackages;

        mkGhcFromScratch = ghcVer:
          mkGhcFrom (overrideHaskellPackages pkgs.haskell.packages.${ghcVer});

        mkGhcSimle = ghcVer:
          mkGhcFrom pkgs.haskell.packages.${ghcVer};

        ghc1 = mkGhcFromScratch "ghc8107";
        ghc2 = mkGhcSimle       "ghc902";
        ghc3 = mkGhcFromScratch "ghc924";
        ghc4 = mkGhcFromScratch "ghc942";

        dockerImage = pkgs.dockerTools.buildLayeredImage {
          name = "playgroundhsenv";
          contents = [ ghc1 ghc2 ghc3 ghc4 pkgs.bash pkgs.coreutils ];
        };

        envVars = workers: timeout: with pkgs; {
          GHC1          = "${ghc1}/bin/ghc";
          GHC2          = "${ghc2}/bin/ghc";
          GHC3          = "${ghc3}/bin/ghc";
          GHC4          = "${ghc4}/bin/ghc";
          DOCKER_IMAGE  = dockerImage;
          SCRIPTS_DIR   = ./scripts;
          WORKERS_COUNT = toString workers;
          DOCKER        = "${docker}/bin/docker";
          TIMEOUT       = "${toString timeout.compiler.term},${toString timeout.compiler.kill},${toString timeout.prog.term},${toString timeout.prog.kill}";
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
              timeout = {
                compiler = {
                  term = mkOption {
                    type = types.int;
                    default = 2;
                  };
                  kill = mkOption {
                    type = types.int;
                    default = 3;
                  };
                };
                prog = {
                  term = mkOption {
                    type = types.int;
                    default = 1;
                  };
                  kill = mkOption {
                    type = types.int;
                    default = 2;
                  };
                };
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
                    Environment = concatStringsSep " " (pkgs.lib.mapAttrsToList (name: value: name + "=" + value) (envVars cfg.workersCount cfg.timeout));
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
        } // envVars 8 {compiler = {term = 2; kill = 3;}; prog = {term = 1; kill = 2;};});
      });
}
