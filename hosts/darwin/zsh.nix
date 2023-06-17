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

          export PATH=/etc/profiles/per-user/$USER/bin:/run/current-system/sw/bin/:$PATH
          # For 1Password CLI. This requires `pkgs.gh` to be installed.
          source $HOME/.config/op/plugins.sh
          # Because, adding it in .ssh/config is not enough.
          # cf. https://developer.1password.com/docs/ssh/get-started#step-4-configure-your-ssh-or-git-client

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

    # Direnv, load and unload environment variables depending on the current directory.
    # https://direnv.net
    # https://rycee.gitlab.io/home-manager/options.html#opt-programs.direnv.enable
    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    #programs.starship.enable = true;
}
