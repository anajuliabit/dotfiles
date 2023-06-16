{
   description = "My first nix flake";

   inputs = {
       # Package sets
       nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
       nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
       # Environment/system management
       darwin.url = "github:lnl7/nix-darwin/master";
       darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
       home-manager.url = "github:nix-community/home-manager";
       home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";        
   };

   
  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:
  let 
    inherit (darwin.lib) darwinSystem;
    inherit (inputs.nixpkgs-unstable.lib) attrValues makeOverridable optionalAttrs singleton;

    # Configuration for `nixpkgs`
    nixpkgsConfig = {
      config = { allowUnfree = true; };
      overlays = attrValues self.overlays ++ singleton (
        # Sub in x86 version of packages that don't build on Apple Silicon yet
        final: prev: (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
          inherit (final.pkgs-x86)
            idris2
            nix-index
            niv
            purescript;
        })
      );
    }; 
  in
  {
    # My `nix-darwin` configs
    darwinConfigurations = rec {
      darwin = darwinSystem {
        system = "aarch64-darwin";
        modules = attrValues self.darwinModules ++ [ 
          # Main `nix-darwin` config
          ./hosts/darwin/configuration.nix
          # `home-manager` module
          home-manager.darwinModules.home-manager
          {
            nixpkgs = nixpkgsConfig;
            # `home-manager` config
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.anajulia = import ./hosts/darwin/home.nix;            
          }
        ];
      };
    }; 

    overlays = {
      # Overlays to add various packages into package set
        comma = final: prev: {
          comma = import inputs.comma { inherit (prev) pkgs; };
        };  

      # Overlay useful on Macs with Apple Silicon
        apple-silicon = final: prev: optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
          # Add access to x86 packages system is running Apple Silicon
          pkgs-x86 = import inputs.nixpkgs-unstable {
            system = "x86_64-darwin";
            inherit (nixpkgsConfig) config;
          };
        }; 
      };

    # My `nix-darwin` modules that are pending upstream, or patched versions waiting on upstream fixes.
    darwinModules = {
      programs-nix-index = 
        # Additional configuration for `nix-index` to enable `command-not-found` functionality with zsh.
        { config, lib, pkgs, ... }:

        {
          config = lib.mkIf config.programs.nix-index.enable {
            programs.zsh.interactiveShellInit = ''
               # This function is called whenever a command is not found.
               command_not_found_handler() {
                 local p=/run/current-system/sw/bin/command-not-found
                 if [ -x $p -a -f /nix/var/nix/profiles/per-user/root/channels/nixos/programs.sqlite ]; then
                   # Run the helper program.
                   $p "$@"
       
                   # Retry the command if we just installed it.
                   if [ $? = 126 ]; then
                     "$@"
                   fi
                 else
                   # Indicate than there was an error so ZSH falls back to its default handler
                   return 127
                 fi
               }
             '';            
         };
       };
     };
  };
}
