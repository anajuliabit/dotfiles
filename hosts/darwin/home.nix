{ pkgs, ... }: 
{
    home.stateVersion = "22.05";
    fonts.fontconfig.enable = true;
    
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

    #programs.command-not-found.enable = false;

    imports = [
        ./git.nix
        ./nvim.nix
        ./tmux.nix
        ./zsh.nix
    ];
}
