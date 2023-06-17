{ pkgs, channel, lib, ... }:

{
  imports = [
       # https://nix-community.github.io/home-manager/index.html#sec-install-nix-darwin-module
       #<home-manager/nix-darwin>
      ./yabai.nix
      ./skhd.nix
  ];


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
