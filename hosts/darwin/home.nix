{ pkgs, ... }: 
{
    home.stateVersion = "22.11";

    fonts.fontconfig.enable = true;
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
    ]);

    programs.home-manager.enable = true;
    programs.nix-index.enable = true;
    programs.nix-index.enableZshIntegration = true;
    programs.zoxide.enable = true; # rust autojump 
    programs.command-not-found.enable = false;

    imports = [
        ./git.nix
        ./nvim/nvim.nix
        ./tmux.nix
        ./zsh.nix
    ];
}
