{ pkgs, ... }:

{
#  environment = {
#    shellAliases = {
#      tx = "tmux new-session -A -s $USER";
#      tmx = "tmux attach-session || tmux";
#      tmux = "tmux -2";
#    };
#    interactiveShellInit = ''
#      ntmx(){
#        # name sessions automatically, depending on the current dir
#        n=$(echo $(basename "$(pwd)") | sed "s/\./_/g")
#        tmux attach-session -t "$n" || tmux new -s "$n"
#      }
#      if test -z $TMUX && [[ $TERM != "screen" ]]; then
#        if [ -n "$BASH_VERSION" ]; then
#          if [[ $- == *i* ]]; then
#            bind '"\C-n":" ntmx\C-m"'
#          fi
#        elif [ -n "$ZSH_VERSION" ]; then
#          bindkey -s '^n' '^qntmx\n'
#        fi
#      fi
#    '';
#  };
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
