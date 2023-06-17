# Doom Emacs: Personally not a fan of github:nix-community/nix-doom-emacs due to performance issues
# This is an ideal way to install on a vanilla NixOS installion.
# You will need to import this from somewhere in the flake (Obviously not in a home-manager nix file)

{ config, pkgs, location, ... }:

{
  services.emacs.enable = true;

  system.userActivationScripts = {               # Installation script every time nixos-rebuild is run. So not during initial install.
    doomEmacs = {
      text = ''
        source ${config.system.build.setEnvironment}
        EMACS="$XDG_CONFIG_HOME/emacs"


        if [ ! -d "$EMACS" ]; then
          ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git $EMACS
          yes | $EMACS/bin/doom install
          rm -r $XDG_CONFIG_HOME/doom
          ln -s ./ $XDG_CONFIG_HOME/doom
          $EMACS/bin/doom sync
        else
          $EMACS/bin/doom sync
        fi
      '';                                        # It will always sync when rebuild is done. So changes will always be applied.
    };
  };

  environment.systemPackages = with pkgs; [
    texlive.combined.scheme-medium
  ];                                             
}
