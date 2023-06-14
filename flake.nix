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
        darwinConfigurations."darkstar" = darwin.lib.darwinSystem {
            system = "x86_64-darwin";
            modules = [ 
                ./hosts/darwin/configuration.nix
                home-manager.darwinModules.home-manager
            ];
        };
    };
}
