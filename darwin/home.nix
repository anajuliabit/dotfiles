{ config, pkgs, lib, inputs, ... }:
let
  hm-config = import ../modules/home-manager.nix {
    config = config;
    pkgs = pkgs;
    lib = lib;
    inputs = inputs;
  };
in {
  imports = [
    <home-manager/nix-darwin>
  ];

  home-manager = {
    useGlobalPkgs = true;
    users.anajulia = { pkgs, lib, ... }:
    lib.recursiveUpdate hm-config {
      programs.command-not-found.enable = true;
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix { };
      home.stateVersion = "22.11";
      home.sessionVariables = {
        EDITOR = "emacs";
        PATH = lib.concatStringsSep ":" [
          "$HOME/.config/emacs/bin"
          "$HOME/Library/Python/3.9/bin"
          "/etc/profiles/per-user/$USER/bin"
          "/run/current-system/sw/bin"
          "$HOME/.local/bin"
        ];
      };
      disabledModules = [ "targets/darwin/linkapps.nix" ];
      # Define a 'buildEnv' environment containing applications.
      # This environment gathers all the applications specified in './packages.nix'.
      home.activation.copyApplications = let
        apps = pkgs.buildEnv {
          name = "home-manager-applications";
          paths = pkgs.callPackage ./packages.nix { };
          pathsToLink = "/Applications";
        };
      # Set the directory where applications will be copied.
      in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        baseDir="$HOME/Applications/Home Manager Apps"
        if [ -d "$baseDir" ]; then
          rm -rf "$baseDir"
        fi
        mkdir -p "$baseDir"
        for appFile in ${apps}/Applications/*; do
          target="$baseDir/$(basename "$appFile")"
          cp -fHRL "$appFile" "$baseDir"
          chmod -R +w "$target"
        done
      '';
    };
  };
}
