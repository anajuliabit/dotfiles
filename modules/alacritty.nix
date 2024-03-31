{ config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      live_config_reload = false;
      font = {
        normal.family = "Iosevka";
        size = 14;
      };
      shell = {
        program = "${pkgs.zsh}/bin/zsh";
        args = [ "--interactive" ];
      };
    };
  };
}
