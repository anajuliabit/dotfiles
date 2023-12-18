{
  description = "My first nix flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";
    foundry.url = "github:shazow/foundry.nix/monthly";
    cairo-nix.url = "github:cairo-nix/cairo-nix";
  };

  outputs =
    { self, darwin, nixpkgs, home-manager, foundry, cairo-nix, ... }@inputs:
    let
      inherit (inputs.nixpkgs-unstable.lib) optionalAttrs;

      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      }));

      nixpkgsConfig = {
        config = {
          allowUnfree = true;
          allowUnsupportedSystem = true;
        };
        overlays = [
          foundry.overlay
          (import cairo-nix)
          emacsOverlay
          #          (final: prev:
          #            optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
          #              inherit (final.pkgs-x86) idris2 nix-index niv;
          #            })
        ];
      };

      darwinConfig = {
        system = "aarch64-darwin";
        modules = [
          ./hosts/darwin/configuration.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs = nixpkgsConfig;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.anajulia = import ./hosts/darwin/home.nix;
          }
        ];
      };

    in {
      darwinConfigurations = { darwin = darwin.lib.darwinSystem darwinConfig; };

      overlays = {
        apple-silicon = final: prev:
          optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            pkgs-x86 = import inputs.nixpkgs-unstable {
              system = "x86_64-darwin";
              inherit nixpkgsConfig;
            };
          };
      };

      darwinModules = {
        programs-nix-index = { config, lib, pkgs, ... }: {
          config = lib.mkIf config.programs.nix-index.enable {
            programs.zsh.interactiveShellInit = ''
              # command_not_found_handler function...
            '';
          };
        };
      };
    };
}
