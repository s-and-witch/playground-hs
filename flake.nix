# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Playground to run haskell code in Telegram";

  inputs = {
    nixpkgs2023-04-17.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs2023-04-17, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs2023-04-17 = nixpkgs2023-04-17.legacyPackages.${system};
        globalPkgs = pkgs2023-04-17;

        foldr1 = fun: list:
          let
            foldr = globalPkgs.lib.foldr;
            nil = {arg = {}; first = true;};
            fun' = head: {arg, first}:
              if first
                then {arg = head; first = false; }
                else {arg = fun head arg; inherit first; };
          in (x: x.arg) (foldr fun' nil list);

        intercalate = sep: foldr1 (h: t: "${h}${sep}${t}" );

        overrideHaskellPackages = hp:
          hp.override { overrides = self: super:
            { mkDerivation = args: super.mkDerivation
                (args // { doCheck = false; doHaddock = false; });
            }; };

        haskellPackages = globalPkgs.haskell.packages.ghc927;

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
            foldl
            massiv
            generic-lens
            vector-algorithms
            Frames
          ];

        mkGhcFromScratchWith = pkgs: ghcVer: packageList:
          let hp = overrideHaskellPackages (pkgs.haskell.packages.${ghcVer});
          in hp.ghcWithPackages packageList;

        mkGhcSimle = pkgs: ghcVer:
          pkgs.haskell.packages.${ghcVer}.ghcWithPackages genericPackages;

        mkGhcFromScratch = pkgs: ghcVer:
          mkGhcFromScratchWith pkgs ghcVer genericPackages;

        ghcs = {
          ghc8107 = mkGhcFromScratch     pkgs2023-04-17 "ghc8107";
          ghc902  = mkGhcSimle           pkgs2023-04-17 "ghc902";
          ghc927  = mkGhcSimle           pkgs2023-04-17 "ghc927";
          ghc944  = mkGhcFromScratch     pkgs2023-04-17 "ghc944";
          ghc961  = mkGhcFromScratchWith pkgs2023-04-17 "ghc961" (_: []);
        };

        ghcDeps =  builtins.attrValues ghcs ++ [globalPkgs.bash globalPkgs.coreutils];

        envVars = {workers ? 8, timeout ? defaultTO, workDir ? "/usr/share/plaground-hs" }:
          with globalPkgs;
            ( builtins.mapAttrs (key: val: "${val}/bin/ghc" ) ghcs ) //
            {
              GHCS          = intercalate "," (builtins.attrNames ghcs);
              DEFAULT_GHC   = "${ghcs.ghc927}/bin/ghc";
              BWRAP         = "${bubblewrap}/bin/bwrap";
              GHC_DEPS      = "${closureInfo { rootPaths = ghcDeps; }}/store-paths";
              SCRIPTS_DIR   = ./scripts;
              WORK_DIR      = workDir;
              WORKERS_COUNT = toString workers;
              TIMEOUT       = intercalate ","  (map (x: toString x) (timeoutToList timeout));
            };

        timeoutToList = timeout:
          [ timeout.compiler.term timeout.compiler.kill timeout.prog.term timeout.prog.kill ];
        defaultTO = {compiler = {term = 2; kill = 3;}; prog = {term = 1; kill = 2;};};

      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            telegram-bot-simple = haskellPackages.callHackageDirect {
              pkg = "telegram-bot-simple";
              ver = "0.6.2";
              sha256 = "sha256-LiGw+ys9+bcy5EL+q5wS7UzwAqqtACHbNrV/GTkwWg4=";
            } {};
          };

        legacyPackages = globalPkgs;

        defaultPackage = self.packages.${system}.${packageName};

        nixosModules.default = with globalPkgs.lib; { config, ... }:
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
              workDir = mkOption {
                type = types.str;
                default = "/usr/share/playground-hs";
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

              systemd.services.playground-hs = {
                wantedBy = [ "multi-user.target" ];
                serviceConfig = {
                    ExecStart = "${self.defaultPackage.${system}}/bin/playground-hs";
                    EnvironmentFile = cfg.envFile;
                    Environment = concatStringsSep " "
                      (globalPkgs.lib.mapAttrsToList (name: value: name + "=" + value)
                      (envVars
                        { workers = cfg.workersCount;
                          timeout = cfg.timeout;
                          workDir = cfg.workDir;
                        }));
                };
              };
            };
          };
        devShell = globalPkgs.mkShell ({
          buildInputs = with globalPkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghc
            cabal-install
            lzma
            zlib
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        } // envVars { workDir = "id-storage"; });
      });
}
