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
       keybindings = [
      { key = "Equals";     mods = "Control";     action = "IncreaseFontSize"; }
      { key = "Add";        mods = "Control";     action = "IncreaseFontSize"; }
      { key = "Subtract";   mods = "Control";     action = "DecreaseFontSize"; }
      { key = "Minus";      mods = "Control";     action = "DecreaseFontSize"; }
      { key = "V";          mods = "Control";     action = "Paste"; }
    ];
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
