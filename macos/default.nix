{ pkgs, channel, lib, config, inputs, ... }:

{
  imports = [
    ./home.nix
  ];

  nix = {
    enable = true;
    package = pkgs.nixVersions.stable;
    gc = {
      automatic = true;
      interval.Day = 7;
      options = "--delete-older-than 7d";
    };
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
    '';
  };

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;
  programs.nix-index.enable = true;
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
    global = {
      lockfiles = true;
    };
    brews = [ "pinentry-mac" "ekhtml" "postgresql@17"  ];
    casks = [ "emacs" "raycast" "grammarly-desktop" "amethyst" "google-chrome" "1password" "slack" "whatsapp" "telegram" ];
    brewPrefix = "/opt/homebrew/bin";
  };
  users.users.anajuliabittencourt = {
    name = "anajuliabittencourt";
    home = "/Users/anajuliabittencourt";
    shell = pkgs.zsh;
  };


  environment.systemPackages = with pkgs; [
    zoxide # fast alternative to autojump and z-lua
    git
    coreutils
    curl
    wget
    vim
    (ripgrep.override { withPCRE2 = true; })
    fd # alternative to find
    fzf
    nix-diff # explain why 2 nix devirations differ
    neofetch # system info cli
    procs # alternative to ps
    sd # alternative to sed
    semgrep
    gnupg
  ];

  system = {
    primaryUser = "anajuliabittencourt";
    stateVersion = 6;
    defaults = {
      NSGlobalDomain = { # Global macOS system settings
        KeyRepeat = 1;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
      };
      dock = { # Dock settings
        autohide = true;
        autohide-delay = 0.0;
        autohide-time-modifier = 0.2;
        expose-animation-duration = 0.5;
        static-only = false;
        show-recents = false;
        show-process-indicators = true;
        mru-spaces = false;
        launchanim = false;
        orientation = "bottom";
        tilesize = 40;
      };
      finder = { 
        QuitMenuItem =
          false; # I believe this probably will need to be true if using spacebar
      };
      trackpad = {
        Clicking = true;
        TrackpadRightClick = true;
      };
      screencapture = {
        location = "~/tmp/screenshots";
      };
    };

   # activationScripts.postActivation.text = ''
   #   # Following line should allow us to avoid a logout/login cycle
   #   /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
   # '';
  };
}
