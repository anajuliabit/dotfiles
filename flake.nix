{
  description = "Ana's dotfiles";

  inputs = 
{
    nixpkgs.url = "github:nixos/nixpkgs/master";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";

    };

    home-manager = {
       url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    foundry.url = "github:shazow/foundry.nix/monthly";
  }
;

  outputs = { self, nix-darwin, nixpkgs, nixpkgs-unstable, home-manager, ...}@inputs: {
      darwinConfigurations = {
        "default" = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./macos
            {
              nixpkgs = {
                overlays = with inputs; [
                  foundry.overlay
                  (final: prev: {
                    claude-code = prev.claude-code.overrideAttrs (_oldAttrs: rec {
                      version = "1.0.32";
                      src = prev.fetchzip {
                        url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
                        hash = "sha256-Awpu3DQz0yi4ZMtgU7JdxpJyWi3j8tyyflgoxO4KLx4=";
                      };
                    });
                  })
                ];
                config = {
                  allowUnfree = true;
                  allowBroken = true;
                  allowUnsupportedSystem = true;
                };
              };
            }
          ];
          specialArgs = { inherit inputs; }; # inputs.self, inputs.nix-darwin, and inputs.nixpkgs can be accessed from the modules
        };
      };
    };
}
