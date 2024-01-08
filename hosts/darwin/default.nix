{ pkgs, changnel, lib, config, inputs, ... }:

{
  imports = [
    ./home.nix
    ../../modules/desktop/skhd.nix
    ../../modules/dev/node.nix
#    ../../modules/desktop/yabai.nix
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
    #brews = [ "koekeishiya/formulae/yabai" ];
    casks = [ "emacs" ];
    brewPrefix = "/opt/homebrew/bin";
  };
  users.users.anajulia = {
    name = "anajulia";
    home = "/Users/anajulia";
    shell = pkgs.zsh;
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
    semgrep
    gnupg
  ];

  launchd.user.agents.emacs.path = [ config.environment.systemPath ];
  launchd.user.agents.emacs.serviceConfig = {
    KeepAlive = true;
    ProgramArguments = [
      "/bin/sh"
      "-c"
      "/bin/wait4path ${pkgs.emacs}/bin/emacs && exec ${pkgs.emacs}/bin/emacs --fg-daemon -q -l ~/nix-dotfiles/modules/emacs/init.el"
    ];
    StandardErrorPath = "/tmp/emacs.err.log";
    StandardOutPath = "/tmp/emacs.out.log";
  };

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
      };
      trackpad = {
        Clicking = true;
        TrackpadRightClick = true;
      };
    };
    activationScripts.applications.text = pkgs.lib.mkForce (''
      echo "setting up ~/Applications..." >&2
      rm -rf ~/Applications/Nix\ Apps
      mkdir -p ~/Applications/Nix\ Apps
      for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
        src="$(/usr/bin/stat -f%Y "$app")"
        cp -r "$src" ~/Applications/Nix\ Apps
      done
    '');
    activationScripts.postUserActivation.text = ''
          # Following line should allow us to avoid a logout/login cycle
          /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
        '';
  };
}
