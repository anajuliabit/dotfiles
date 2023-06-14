{ pkgs, ... }:

{
    programs.tmux = {
        enable = true;
        shortcut = "a";
        keyMode = "vi";
        plugins = with pkgs.tmuxPlugins; [
            pain-control
            # gruvbox
        ];
    };
}
