{
  description = "Ana's darwin system";

  inputs = 
{
    # pinned darwin
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # pinned home-manager
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    foundry.url = "github:shazow/foundry.nix/monthly";
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
          specialArgs = { inherit inputs; };
        };
      };
    };
}
