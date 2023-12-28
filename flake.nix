{
  description = "My dotfiles with nix";

  inputs = {
    #flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    #nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    foundry.url = "github:shazow/foundry.nix/monthly";
    #cairo-nix.url = "github:cairo-nix/cairo-nix";
  };

  outputs = { self, darwin, nixpkgs, home-manager, foundry, ... }@inputs:
    let
      overlays = [
        foundry.overlay
        #(import cairo-nix)
      ];
    in {
      darwinConfigurations = {
        darwin = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./hosts/darwin
            {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                  allowBroken = true;
                  allowUnsupportedSystem = true;
                };
                overlays = overlays;
              };
            }
          ];
          inputs = { inherit darwin home-manager nixpkgs; };
        };
      };

    };
}
