{ config, pkgs, lib, inputs, ... }:

{
  imports = [ ./dev/git.nix ./dev/tmux.nix ./dev/zsh.nix ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
