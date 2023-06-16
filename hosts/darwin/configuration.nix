{ pkgs, channel, ... }:

{
  imports = [
       # https://nix-community.github.io/home-manager/index.html#sec-install-nix-darwin-module
       #<home-manager/nix-darwin>
      ./yabai.nix
      ./skhd.nix
  ];

#  home-manager.useUserPackages = true;
#  home-manager.useGlobalPkgs = true;
 # home.users.anajulia = import ./home.nix;
  # inputs.home.useUserPackages = true;
 # inputs.home.useGlobalPkgs = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  
  nix = {
    package = pkgs.nix;
    gc = {                                
      automatic = true;
      interval.Day = 7;
      options = "--delete-older-than 7d";
    };
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
    '';
  #  nixPath = [
  #    "nixpkgs=${channel.input}"
  #    "home-manager=${home}"
  # ];

  };

  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    casks = [
        # "amethyst"
        "discord"
        #"iterm2"
        "spotify"
    ];
  };

  users.users.anajulia= {
      name = "anajulia";
      home = "/Users/anajulia";
  };

  environment.systemPackages = with pkgs; [ 
     # Terminal
     #ansible
     git
     #fast alternative ot autojump and z-lua
     #ranger
     vim
     ripgrep
     fd # alternative to find
     curl
     wget
     fzf
     nix-diff # explain why 2 nix devirations differ
     #emacs
     neofetch # system info cli
     yubikey-manager 
     procs # alternative to ps
     sd # alternative to sed
     tealdeer # tldr implementation for simplified example based man pages
     exa # Replacement for ls
   ];

    }
