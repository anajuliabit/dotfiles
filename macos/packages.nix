{ pkgs }:
let
glibtool = pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'';
safe-cli = pkgs.writeShellScriptBin "safe-cli" ''
  VENV_DIR="$HOME/.local/safe-cli-venv"
  
  if [ ! -d "$VENV_DIR" ]; then
    echo "Setting up safe-cli virtual environment..."
    ${pkgs.python312}/bin/python -m venv "$VENV_DIR"
    source "$VENV_DIR/bin/activate"
    pip install --upgrade pip
    pip install --upgrade "safe-cli[ledger]"
  fi
  
  # Always activate the virtual environment and set PYTHONPATH
  source "$VENV_DIR/bin/activate"
  export PYTHONPATH="$VENV_DIR/lib/python3.12/site-packages:$PYTHONPATH"
  
  exec "$VENV_DIR/bin/safe-cli" "$@"
'';
in (with pkgs; [
  docker
  tldr
  nerd-fonts.droid-sans-mono
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
  libtool
  nodePackages.typescript
  nodePackages.typescript-language-server
  nodePackages_latest.nodejs
  nodePackages.prettier
  bun
  yarn
  comma # run software from without installing it
  languagetool
  graphviz
  gnuplot
  python312
  safe-cli
  lcov
  #appimage-run
  #gnumake
  #gimp
  fontconfig
  #galculator 
  # rofi-calc
  #direnv
  #chromedriver
  #rnix-lsp # lsp-mode for nix
  #qmk
  tree-sitter
  #unixtools.ifconfig
  #xorg.xrandr
  #inotify-tools # inotifywait, inotifywatch - For file system events
  #libnotify
  #pinentry-curses
  sqlite
  xdg-utils
  #xdotool
  #google-chrome
  discord
  spotify
  iterm2
  solc-select
  vscode
  eza # ls replacement
  # claude-code # Already included with nodejs
  # ledger-live-desktop # Install manually from https://www.ledger.com/ledger-live
  # Nix 
  nixfmt-rfc-style 
  niv # easy dependency management for nix projects
] ++ (with pkgs.darwin.apple_sdk.frameworks; [ SystemConfiguration ]))
