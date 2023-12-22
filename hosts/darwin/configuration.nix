{ pkgs, channel, lib, config, inputs, ... }:

{
  imports = [
    ../../modules/desktop/yabai.nix
    ../../modules/desktop/skhd.nix
    ../../modules/emacs/doom/doom.nix
    ../../modules/dev/node.nix
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
    '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    #  nixPath = [
    #    "nixpkgs=${channel.input}"
    #    "home-manager=${home}"
    # ];
  };

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;
  programs.nix-index.enable = true;

  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    casks = [
      # "amethyst"
      "discord"
      #"iterm2"
      "spotify"
      "ledger-live"
      "zoom"
    ];
    brews = [ "libusb" ];
  };

  users.users.anajulia = {
    name = "anajulia";
    home = "/Users/anajulia";
  };

  #fonts.fontconfig.enable = true;

  environment.systemPackages = with pkgs; [
    #ansible
    git
    zoxide # fast alternative ot autojump and z-lua
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
    exa # Replacement for ls
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
      finder = { # Finder settings
        QuitMenuItem =
          false; # I believe this probably will need to be true if using spacebar
        #        ShowExternalHardDrivesOnDesktop = true;
        #        ShowHardDrivesOnDesktop = true;
        #        ShowMountedServersOnDesktop = true;
        #        ShowRemovableMediaOnDesktop = true;
        #        _FXSortFoldersFirst = true;
      };
      trackpad = { # Trackpad settings
        Clicking = true;
        TrackpadRightClick = true;
      };
    };
    activationScripts.postUserActivation.text = ''
      # Following line should allow us to avoid a logout/login cycle
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';
  };

  nixpkgs.config.permittedInsecurePackages = [ "nodejs-16.20.1" ];
}
