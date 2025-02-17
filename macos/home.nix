{ config, pkgs, lib, inputs, ... }:
let
  hm-config = import ../modules/home-manager.nix {
    config = config;
    pkgs = pkgs;
    lib = lib;
    inputs = inputs;
  };
in {
  imports = [
    <home-manager/nix-darwin>
  ];

  home-manager = {
    useGlobalPkgs = true;
    users.anajuliabittencourt = { pkgs, lib, ... }:
    lib.recursiveUpdate hm-config {
      programs.command-not-found.enable = true;
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix { };
      home.stateVersion = "22.11";
      
      programs.gpg = {
        enable = true;
        settings = {
          use-agent = true;
        };
        scdaemonSettings = {
          "reader-port" = "Yubico YubiKey";
          "disable-ccid" = true;
          "card-timeout" = "5";
          "log-file" = "~/.gnupg/scdaemon.log";
          "debug-level" = "basic";
        };
        mutableKeys = true;
        mutableTrust = true;
      };

      # GPG agent configuration for macOS
      home.file.".gnupg/gpg-agent.conf".text = ''
        enable-ssh-support
        default-cache-ttl 60
        max-cache-ttl 120
        pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
      '';

      # Add GPG agent to LaunchAgents to start at login
      home.file."Library/LaunchAgents/org.gnupg.gpg-agent.plist".text = ''
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" 
          "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
        <dict>
          <key>Label</key>
          <string>org.gnupg.gpg-agent</string>
          <key>Program</key>
          <string>${pkgs.gnupg}/bin/gpg-agent</string>
          <key>ProgramArguments</key>
          <array>
            <string>${pkgs.gnupg}/bin/gpg-agent</string>
            <string>--daemon</string>
          </array>
          <key>RunAtLoad</key>
          <true/>
        </dict>
        </plist>
      '';

      programs.zsh.initExtra = ''
        export GPG_TTY="$(tty)"
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
        gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1
      '';

      home.sessionVariables = {
        LSP_USE_PLISTS = "true";
        EDITOR = "vim";
        GPG_TTY = "$(tty)";
        SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
        PATH = lib.concatStringsSep ":" [
          "$HOME/.config/emacs/bin"
          "$HOME/.foundry/bin"
          "$HOME/.gnupg"
          "$HOME/Library/Python/3.9/bin"
          "$HOME/.local/bin"
          "/run/current-system/sw/bin"
          "/etc/profiles/per-user/$USER/bin"
          "NIX_LDFLAGS=-L${pkgs.libiconv}/lib"
          "$PATH"
          "$HOME/bin"
        ];
      };
    };
  };
}
