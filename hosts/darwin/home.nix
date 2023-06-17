{ pkgs, ... }: 
{
    home.stateVersion = "22.05";
    fonts.fontconfig.enable = true;
    # Direnv, load and unload environment variables depending on the current directory.
    # https://direnv.net
    # https://rycee.gitlab.io/home-manager/options.html#opt-programs.direnv.enable
    
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
     #emacs
     # nodePackages.typescript
     # nodejs

      yubikey-manager 

      ] ++ lib.optionals stdenv.isDarwin [
        m-cli # useful macOS CLI commands
      ];

    #programs.nix-index.enable = true;
    #programs.nix-index.enableZshIntegration = true;
    #programs.zoxide.enable = true; # rust autojump 
    #programs.command-not-found.enable = false;

    imports = [
        ./git.nix
        ./nvim/nvim.nix
        ./tmux.nix
        ./zsh.nix
    ];
}
