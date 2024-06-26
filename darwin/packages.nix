{ pkgs }:
let
glibtool = pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'';
in (with pkgs; [
  tldr
  (nerdfonts.override { fonts = [ "Cousine" ]; })
  htop
  httpie
  jq
  yq
  m-cli # useful macOS CLI commands
  _1password-gui
  yubikey-manager
  yubikey-agent
  silver-searcher
  pkg-config
  rustup
  #rust-analyzer
  libiconv # charset conversion
  rnix-lsp
  nixfmt
  niv # easy dependency management for nix projects
  libtool # for Emacs vterm
  # node packages
  nodePackages.typescript
  nodejs
  nodePackages.node2nix
  nodePackages.pnpm
  nodePackages.prettier
  comma # run software from without installing it
  languagetool
  graphviz
  gnuplot
  python3
  #zotero
  #appimage-run
  #gnumake
  home-manager
  #gimp
  #vlc
  #wineWowPackages.stable
  fontconfig
  #galculator
  #chromedriver
  #direnv
  #rofi-calc
  #rnix-lsp # lsp-mode for nix
  #qmk
  #feh # Manage wallpapers
  #screenkey
  tree-sitter
  #unixtools.ifconfig
  #unixtools.netstat
  #xclip # For the org-download package in Emacs
  #xorg.xrandr
  #inotify-tools # inotifywait, inotifywatch - For file system events
  #i3lock-fancy-rapid
  #libnotify
  #pinentry-curses
  sqlite
  xdg-utils
  #yad # I use yad-calendar with polybar
  #xdotool
  #google-chrome
  tdesktop 
  discord
  spotify
  pkgs.texlive.combined.scheme-full # All of texlive (including LaTeX and ConTEXt)
  pkgs.texlab # LSP for LaTeX
] ++ (with pkgs.darwin.apple_sdk.frameworks; [ SystemConfiguration ]))
