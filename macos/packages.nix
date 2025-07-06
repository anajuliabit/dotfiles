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
claude-code = pkgs.writeShellScriptBin "claude-code" ''
  NPM_CONFIG_PREFIX="$HOME/.local/npm"
  export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
  
  if [ ! -f "$NPM_CONFIG_PREFIX/bin/claude" ]; then
    echo "Installing claude-code via npm..."
    mkdir -p "$NPM_CONFIG_PREFIX"
    ${pkgs.nodePackages_latest.nodejs}/bin/npm install --prefix "$NPM_CONFIG_PREFIX" -g @anthropic-ai/claude-code
  else
    echo "Updating claude-code..."
    ${pkgs.nodePackages_latest.nodejs}/bin/npm update --prefix "$NPM_CONFIG_PREFIX" -g @anthropic-ai/claude-code
  fi
  
  exec "$NPM_CONFIG_PREFIX/bin/claude" "$@"
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
  claude-code
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
  # ledger-live-desktop # Install manually from https://www.ledger.com/ledger-live
  # Nix 
  nixfmt-rfc-style 
  niv # easy dependency management for nix projects
] ++ (with pkgs.darwin.apple_sdk.frameworks; [ SystemConfiguration ]))
