{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ yabai ];

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;
    config = {
      #focus_follows_mouse = "autoraise";
      #mouse_follows_focus = "on";
      #external_bar = "all:24:0";
      #window_opacity = "on";
      #window_opacity_duration = "0.0";
      #window_border = "off";
      #window_border_placement = "inset";
      # window_border_width = 2;
      # window_border_radius = 3;
      # active_window_border_topmost = "off";
      #window_topmost = "on";
      #window_shadow = "float";
      #active_window_border_color = "0xff5c7e81";
      #normal_window_border_color = "0xff505050";
      #insert_window_border_color = "0xffd75f5f";
      #active_window_opacity = "1.0";
      #normal_window_opacity = "1.0";
      #auto_balance = "on";

      focus_follows_mouse = "off";
      mouse_follows_focus = "off";
      window_placement = "second_child";
      window_topmost = "off";
      window_opacity = "off";
      window_opacity_duration = "0.0";
      window_shadow = "on";
      window_border = "on";
      window_border_width = 4;
      active_window_opacity = "1.0";
      normal_window_opacity = "0.90";
      split_ratio = "0.50";
      auto_balance = "off";
      mouse_modifier = "alt";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      layout = "bsp";
      top_padding = 3;
      bottom_padding = 6;
      left_padding = 6;
      right_padding = 6;
      window_gap = 6;

    };
    extraConfig = ''
      yabai -m rule --add app="System Preferences" manage=off
      yabai -m rule --add app="Activity Monitor" manage=off
      yabai -m rule --add app="Font Book" manage=off
      yabai -m rule --add app="App Store" manage=off
      yabai -m rule --add app="System Information" manage=off
      yabai -m rule --add app="^Emacs$" title!='^$' manage=on


      # Taken from https://github.com/koekeishiya/yabai/issues/719#issuecomment-728140216

      # Focus window after active space changes
      #yabai -m signal --add event=space_changed \
      # action="yabai -m window --focus \$(yabai -m query --windows --space | jq .[0].id)"

      # Focus window after active display changes
      #yabai -m signal --add event=display_changed \
      #  action="yabai -m window --focus \$(yabai -m query --windows --space | jq .[0].id)"

      # load scripting additions
      sudo yabai --load-sa
      yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
    '';
  };
}
