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
