{ config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      live_config_reload = false;
      font = {
         family =  "Iosevka";
        size = 20;
      };
      shell = {
        program = "${pkgs.zsh}/bin/zsh";
        args = [ "--interactive" ];
      };

      # Enable Vi mode
      key_bindings = [
        {
          key = "V";
          mods = "Control";
          action = { ToggleViMode = {}; };
        }
      ];
    };
  };
}
