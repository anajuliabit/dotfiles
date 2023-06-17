{ pkgs, ... }: 
{
    home.stateVersion = "22.05";
    fonts.fontconfig.enable = true;
    # Direnv, load and unload environment variables depending on the current directory.
    # https://direnv.net
    # https://rycee.gitlab.io/home-manager/options.html#opt-programs.direnv.enable
    
    home.packages = (with pkgs; [
      (nerdfonts.override { fonts = [ "Cousine" ]; })
      htop
      httpie
      jq
      yq
      ripgrep

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

      tldr
     # neovim
    ]);

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
