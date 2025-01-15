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
      };

      home.file.".gnupg/gpg-agent.conf".text = ''
        enable-ssh-support
        default-cache-ttl 60
        max-cache-ttl 120
        pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
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
        ];
      };
    };
  };
}
