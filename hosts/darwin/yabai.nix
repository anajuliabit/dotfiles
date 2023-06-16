{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yabai
  ];

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;
    config = {
      focus_follows_mouse          = "off";
      mouse_follows_focus          = "on";
      external_bar                 = "all:24:0";
      window_placement             = "second_child";
      window_opacity               = "on";
      window_opacity_duration      = "0.0";
      window_border                = "on";
      window_border_placement      = "inset";
      window_border_width          = 2;
      window_border_radius         = 3;
      active_window_border_topmost = "off";
      window_topmost               = "on";
      window_shadow                = "float";
      active_window_border_color   = "0xff5c7e81";
      normal_window_border_color   = "0xff505050";
      insert_window_border_color   = "0xffd75f5f";
      active_window_opacity        = "1.0";
      normal_window_opacity        = "0.8";
      split_ratio                  = "0.50";
      auto_balance                 = "on";
      mouse_modifier               = "alt";
      mouse_action1                = "move";
      mouse_action2                = "resize";
      layout                       = "bsp";
      top_padding                  = 6;
      bottom_padding               = 6;
      left_padding                 = 6;
      right_padding                = 6;
      window_gap                   = 6;
    };
    extraConfig = ''
    yabai -m rule --add app="System Preferences" manage=off
    yabai -m rule --add app="Activity Monitor" manage=off
    yabai -m rule --add app="Font Book" manage=off
    yabai -m rule --add app="App Store" manage=off
    yabai -m rule --add app="System Information" manage=off

    yabai -m rule --add title="floating" manage=off

    # Taken from https://github.com/koekeishiya/yabai/issues/719#issuecomment-728140216

    # Focus window after active space changes
    yabai -m signal --add event=space_changed \
      action="yabai -m window --focus \$(yabai -m query --windows --space | jq .[0].id)"

    # Focus window after active display changes
    yabai -m signal --add event=display_changed \
      action="yabai -m window --focus \$(yabai -m query --windows --space | jq .[0].id)"
    
    # 
    yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
    yabai --load-sa
  '';
  };
}