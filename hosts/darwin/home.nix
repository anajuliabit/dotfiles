{ pkgs, ... }: 
{
    home.stateVersion = "22.05";
    
    home.packages = with pkgs; [
      tldr
      (nerdfonts.override { fonts = [ "Cousine" ]; })
      htop
      httpie
      jq
      yq

      # rust
      rustc
      cargo
      rust-analyzer

      # nix
      rnix-lsp 
      nixfmt

      # lua
      lua
      sumneko-lua-language-server 
      stylua

     # neovim
     # nodePackages.typescript
     # nodejs

      yubikey-manager 
      rtx

      ] ++ lib.optionals stdenv.isDarwin [
        m-cli # useful macOS CLI commands
      ];

    #programs.command-not-found.enable = false;

    imports = [
        ./git.nix
        ./nvim.nix
        ./tmux.nix
        ./zsh.nix
       # ../../modules/emacs
    ];

#  home.sessionVariables = rec {
#    XDG_CACHE_HOME = "\${HOME}/.cache";
#    XDG_CONFIG_HOME = "\${HOME}/.config";
#    XDG_BIN_HOME = "\${HOME}/.local/bin";
#    XDG_DATA_HOME = "\${HOME}/.local/share";
#  };

  #  PATH = [
  #    "\${HOME}/.bin"
  #    "\${XDG_BIN_HOME}"
  #    "\${HOME}/.node_modules"
  #  ];

#   modules.emacs.enable = true;
}
