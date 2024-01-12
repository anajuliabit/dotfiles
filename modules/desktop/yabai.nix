{ config, pkgs, lib, ... }: let
in {
  home.file.yabai = {
    executable = true;
    target = ".config/yabai/yabairc";
    text = ''
#!/usr/bin/env sh

# load scripting additions
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
sudo yabai --load-sa

# config
yabai -m config layout bsp
yabai -m config top_padding    8
yabai -m config bottom_padding 8
yabai -m config left_padding   8
yabai -m config right_padding  8
yabai -m config window_gap     8
yabai -m config auto_balance off
yabai -m config split_ratio 0.5
yabai -m config window_shadow off

yabai -m config insert_feedback_color 0xffd75f5f
yabai -m config active_window_border_color 0xff37474F
yabai -m config normal_window_border_color 0xffECEFF1
yabai -m config window_border_width 2
yabai -m config window_border_radius 0
yabai -m config window_border_blur off
yabai -m config window_border_hidpi on
yabai -m config window_border off

#
# setup spaces
#

function setup_space {
  local idx="$1"
  local name="$2"
  local space=
  echo "setup space $idx : $name"

  space=$(yabai -m query --spaces --space "$idx")
  if [ -z "$space" ]; then
    yabai -m space --create
  fi

  yabai -m space "$idx" --label "$name"
}

setup_space 1 emacs
setup_space 2 eeb
setup_space 3 social
setup_space 4 other

# floating apps and windows
yabai -m rule --add app="^System Preferences$" manage=off
yabai -m rule --add app="^Emacs$" title!='^$' manage=on

# move some apps automatically to specific spaces
yabai -m rule --add app="^Brave$" space=2
yabai -m rule --add app="^Telegram$" space=3
yabai -m rule --add app="^Spotify$" space=4
      '';
  };

  home.file.skhd = {
    target = ".config/skhd/skhdrc";
    text = ''

################################################################################
#
# window manipulation
#
alt - j : yabai -m window --focus south
alt - k : yabai -m window --focus north
alt - h : yabai -m window --focus west
alt - l : yabai -m window --focus east

# move window to space
shift + alt - 1 : yabai -m window --space ; 
shift + alt - 2 : yabai -m window --space ; 
shift + alt - 3 : yabai -m window --space ;
shift + alt - 4 : yabai -m window --space ; 

################################################################################
#
# space manipulation
#

# space focus
ctrl - 1 : yabai -m space --focus 1
ctrl - 2 : yabai -m space --focus 2
ctrl - 3 : yabai -m space --focus 3
ctrl - 4 : yabai -m space --focus 4

################################################################################
#
# window manipulation
#

alt - return : yabai -m window --swap first

################################################################################
#
# Applications
#

shift + alt - c [
  "emacs" : skhd -k "ctrl - x" ; skhd -k "ctrl - c"
  *       : skhd -k "cmd - q"
]

################################################################################
#
# Mode for opening applications
#

:: open @
alt - o ; open
open < alt - o ; default 

# emacs
open < e : nohup emacs &>/dev/null & ; skhd -k "alt - o"
open < shift - e : nohup emacs --debug-init &>/dev/null & ; skhd -k "alt - o"

# alacritty
#open < return : open -na ${pkgs.alacritty}/Applications/Alacritty.app ; skhd -k "alt - o"
#shift + alt - return : open -na ${pkgs.alacritty}/Applications/Alacritty.app
      '';
  };
}
