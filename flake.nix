{
    description = "My first nix flake";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";

        darwin = {
            url = "github:LnL7/nix-darwin";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { self, nixpkgs, darwin, home-manager }: {
        darwinConfigurations."darwin" = darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [ 
                # force nix to use flake registry instead of channel
                # https://github.com/nix-community/nix-index/issues/167 
                { nix.nixPath.nixpkgs = "${nixpkgs}"; }
                ./hosts/darwin/configuration.nix
                home-manager.darwinModules.home-manager
            ];
        };
    };
}
