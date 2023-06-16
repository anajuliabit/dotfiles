{ pkgs, ... }:

{
    programs.zsh = {
        enable = true;
        plugins = [
            {
                name = "alias-tips";
                src = pkgs.fetchFromGitHub {
                    owner = "djui";
                    repo = "alias-tips";
                    rev = "41cb143ccc3b8cc444bf20257276cb43275f65c4";
                    sha256 = "ZFWrwcwwwSYP5d8k7Lr/hL3WKAZmgn51Q9hYL3bq9vE=";
                };
            }
        ];
        enableAutosuggestions = true;
        autocd = true;
        dirHashes = {
          dl = "$HOME/Downloads";
          docs = "$HOME/Documents";
          code = "$HOME/dev/code";
          pics = "$HOME/Pictures";
        };
        dotDir = ".config/zsh";
       # history = {
       #   expireDuplicatesFirst = true;
       #   path = "${config.xdg.dataHome}/zsh_history";
       # };

        initExtra = ''
          # search history based on what's typed in the prompt
          autoload -U history-search-end
          zle -N history-beginning-search-backward-end history-search-end
          zle -N history-beginning-search-forward-end history-search-end
          bindkey "^[OA" history-beginning-search-backward-end
          bindkey "^[OB" history-beginning-search-forward-end

          # case insensitive tab completion
          zstyle ':completion:*' completer _complete _ignored _approximate
          zstyle ':completion:*' list-colors '\'
          zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
          zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
          zstyle ':completion:*' menu select
          zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
          zstyle ':completion:*' verbose true
          _comp_options+=(globdots)
        '';

        shellAliases = {
          ll = "ls -l";
          grep = "grep --color";
          ip = "ip --color";
          l = "exa -l";
          la = "exa -la";
          md = "mkdir -p";
	      nvim = "nvim -u ~/.config/nvim-bkp/init.lua"; # fix this when finish nvim migration 
        };
        shellGlobalAliases = {exa = "exa --icons --git";};
        oh-my-zsh = {
            enable = true;
            plugins = [
                "git"
                "vi-mode"
            ];
        };
    };
    #programs.starship.enable = true;
}
