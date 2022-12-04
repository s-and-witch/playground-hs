# playground-hs

In-telegram feature-rich playground for Haskell with libraries support.

## Deployment

Warning: the first building of the environment can take more than 1 hour due to the huge lack of caches.

Add new input into `/etc/nixos/flake.nix`, use provided module, and configure playground-hs:

```nix
{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    playground-hs.url = "github:Player-205/playground-hs";
  };

  outputs = { self, nixpkgs, playground-hs, ... }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules =
        [ playground-hs.nixosModules.${system}.default
          {
            services.playground-hs = {
              enable = true;
              envFile = "/path/to/the/environment/file.env";
              workersCount = 8; # optional
              timeout = { # optional
                compiler = {
                  term = 2;
                  kill = 3;
                };
                prog = {
                  term = 1;
                  kill = 2;
                };
              };
            };
          }
          ...
        ];
    };
  };
}
```
The environment file should contain a telegram bot token in the following form:
```
TG_TOKEN=...
```

## Usage

You can play with this playground, using https://t.me/LaTelagrambdaBot

A command should be in this form:
```
/runhaskell <ARGS>
-- haskell code
-- haskell code
```
Where \<ARGS\> are
- GHC version: ghc8107|ghc902(default)|ghc924|ghc942
- Action: core|run(default)
- Optimistaion level: O0(default)|O1|O2
