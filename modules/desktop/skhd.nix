{ config, pkgs, ... }:


{
#  services.skhd = {
#    enable = true;
#    skhdConfig = ''
#    # change window focus within space
#    alt - j : yabai -m window --focus south
#    alt - k : yabai -m window --focus north
#    alt - h : yabai -m window --focus west
#    alt - l : yabai -m window --focus east
#
#    # change focus between external displays
#    #"shift + alt - j: yabai -m display --focus north"
#    #"shift + alt - k: yabai -m display --focus south"
#
#    # rotate layout
#    shift + alt - r : yabai -m space --rotate 270
#    shift + alt - y : yabai -m space --mirror y-axis
#    shift + alt - x : yabai -m space --mirror x-axis
#    shift + alt - t : yabai -m window --toggle float --grid 4:4:1:1:2:2
#
#    # move window to prev and next space
#    shift + alt - p : yabai -m window --space prev;
#    shift + alt - n : yabai -m window --space next;
#
#    # move window to space #
#    shift + alt - 1 : yabai -m window --space 1;
#    shift + alt - 2 : yabai -m window --space 2;
#    shift + alt - 3 : yabai -m window --space 3;
#    shift + alt - 4 : yabai -m window --space 4;
#
#    # move focus to next and prev space
#    alt + shift - l : yabai -m space --focus next
#    alt + shift - h : yabai -m space --focus prev
#
#    # change window focus within space
#    ctrl - 1 : yabai -m space --focus 1
#    ctrl - 2 : yabai -m space --focus 2
#    ctrl - 3 : yabai -m space --focus 3
#    ctrl - 4 : yabai -m space --focus 4
#
#    # emacs everywhere
#    "shift + alt + control : emacsclient --eval '(emacs-everywhere)'"
#  '';
#  };

home.file.skhd = {
  target = ".config/skhd/skhdrc";
  text = ''

################################################################################
#
# space manipulation

# space focus
ctrl - 1 : yabai -m space --focus 1
ctrl - 2 : yabai -m space --focus 2
ctrl - 3 : yabai -m space --focus 3
ctrl - 4 : yabai -m space --focus 

################################################################################
#
# window manipulation
#

alt-j : yabai -m window --focus south
alt-k : yabai -m window --focus north
alt-h : yabai -m window --focus west
alt-l : yabai -m window --focus east

################################################################################
#
# Applications
#

shift + alt - c [
  "emacs" : skhd -k "ctrl - x" ; skhd -k "ctrl - c"
  *       : skhd -k "cmd - q"
]

'';
};
}
