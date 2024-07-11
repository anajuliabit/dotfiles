{ pkgs, changnel, lib, config, inputs, ... }:

{
  imports = [
    ./home.nix
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix = {
    package = pkgs.nixUnstable;
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
    onActivation.autoUpdate = true;
    brews = [ "pinentry-mac" "ekhtml" ]; 
    casks = [ "emacs" "mactex" "amethyst"];
    brewPrefix = "/opt/homebrew/bin";
  };
  users.users.anajulia = {
    name = "anajulia";
    home = "/Users/anajulia";
    shell = pkgs.zsh;
  };

  #fonts.fontconfig.enable = true;
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
        expose-animation-duration = 0.2;
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
    };
    activationScripts.postUserActivation.text = ''
          # Following line should allow us to avoid a logout/login cycle
          /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
        '';
  };
}
