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
    inputs.home-manager.darwinModules.home-manager
  ];

  home-manager = {
    useGlobalPkgs = true;
    backupFileExtension = "bak";
    # Override the lib module to include mdDoc for home-manager modules
    users.anajuliabittencourt = { pkgs, lib, ... }:
    lib.recursiveUpdate hm-config {
      programs.command-not-found.enable = true;
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix { };
      # Personal configuration shared between `nix-darwin` and plain `home-manager` configs.
      # This value determines the Home Manager release that your configuration is compatible with. This
      # helps avoid breakage when a new Home Manager release introduces backwards incompatible changes.
      #
      # You can update Home Manager without changing this value. See the Home Manager release notes for
      # a list of state version changes in each release.
      home.stateVersion = "23.05";
      
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

      home.sessionVariables = {
        LSP_USE_PLISTS = "true";
        EDITOR = "vim";
        GPG_TTY = "$(tty)";
        SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
        PATH = lib.concatStringsSep ":" [
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
