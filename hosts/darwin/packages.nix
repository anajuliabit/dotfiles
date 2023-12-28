{ pkgs }:

with pkgs; [
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
  # dev tools
  gdb
  #clang-tools
  #clang
  #cmake
  #ripgrep
  pkg-config
  rustup
  #rust-analyzer
  iconv
  #libiconv
  rnix-lsp
  nixfmt
  niv # easy dependency management for nix projects
  libtool # for Emacs vterm
  nodePackages.typescript
  nodejs
  nodePackages.node2nix
  comma # run software from without installing it
  ledger
  ispell
  hunspell
  hunspellDicts.en-us
  hunspellDicts.pt-br
  languagetool
  graphviz
  gnuplot
  nim
  python3
  foundry-bin
  #cairo-bin.stable.scarb
  #zotero
  #appimage-run
  #gnumake
  home-manager
  #gimp
  #vlc
  #wineWowPackages.stable
  fontconfig
  #galculator
  #tdesktop # telegram desktop
  #chromedriver
  #direnv
  #rofi
  #rofi-calc
  #rnix-lsp # lsp-mode for nix
  #qmk
  #emote # Emoji picker
  #feh # Manage wallpapers
  #screenkey
  #tree
  #unixtools.ifconfig
  #unixtools.netstat
  #xclip # For the org-download package in Emacs
  #xorg.xrandr
  #inotify-tools # inotifywait, inotifywatch - For file system events
  #i3lock-fancy-rapid
  #libnotify
  #ledger-live-desktop
  #playerctl # Control media players from command line
  #pinentry-curses
  #pcmanfm # Our file browser
  sqlite
  xdg-utils
  #yad # I use yad-calendar with polybar
  #xdotool
  #google-chrome
  discord
  spotify
]
