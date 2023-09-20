{ pkgs, lib, ... }: {
  home.stateVersion = "22.05";

  home.packages = with pkgs;
    [
      tldr
      (nerdfonts.override { fonts = [ "Cousine" ]; })
      htop
      httpie
      jq
      yq

      # dev tools
      gdb
      #clang-tools
      #clang
      #cmake
      #ripgrep
      pkg-config

      # rust
      rustup
      #rust-analyzer
      iconv
      libiconv

      # nix
      rnix-lsp
      nixfmt
      niv # easy dependency management for nix projects

      # lua
      lua
      sumneko-lua-language-server
      stylua

      nodePackages.typescript
      nodejs

      yubikey-manager
      nodePackages.node2nix
      comma # run software from without installing it

      ledger
      # grammar
      ispell
      hunspell
      hunspellDicts.en-us
      hunspellDicts.pt-br
      languagetool

      # latex
      texlive.combined.scheme-full

      graphviz

      #foundry-bin
      #zotero
    ] ++ lib.optionals stdenv.isDarwin [
      m-cli # useful macOS CLI commands
    ];

  home.sessionVariables = {
    EDITOR = "emacs";
    PATH =
      "/Users/anajulia/.config/emacs/bin:/Users/$USER/Library/Python/3.9/bin:$PATH";
    CXX = "clang++";
    LIBRARY_PATH = "${
        lib.makeLibraryPath [ pkgs.libiconv ]
      }\${LIBRARY_PATH:+:$LIBRARY_PATH}";
  };

  programs.home-manager.enable = true;
  programs.command-not-found.enable = true;
  programs.rtx.enable = true;

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
