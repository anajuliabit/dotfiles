{ config, pkgs, lib, inputs, ... }:

{
  imports = [./dev/git.nix ./dev/zsh.nix ./dev/emacs.nix ./desktop/skhd.nix];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
