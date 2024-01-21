{
  description = "My dotfiles with nix";

  inputs = 
{
    #flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
  }
;

  outputs = { self, darwin, flake-utils, nixpkgs, nixpkgs-unstable, home-manager, foundry, ... }@inputs:
    let
      pkgs = import nixpkgs { system = "aarch64-darwin"; };
      overlays = [
        foundry.overlay
        #(import ./overlays)
      ] ;
    in {
      darwinConfigurations = {
        macbook = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./darwin
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
