{ config, pkgs, lib, inputs, ... }:

{
  imports = [ ./git.nix ./zsh.nix ./alacritty.nix ];
}
