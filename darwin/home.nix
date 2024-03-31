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
          "$HOME/.config/.foundry/bin"
          "$HOME/Library/Python/3.9/bin"
          "$HOME/.local/bin"
          "/run/current-system/sw/bin"
          "/etc/profiles/per-user/$USER/bin"
          "$PATH"
        ];
      };
    };
  };
}
