{ config, pkgs, inputs, ... }:

{
  environment.systemPackages = with pkgs; [
    ((emacsPackagesFor emacs-unstable).emacsWithPackages
      (epkgs: [ epkgs.vterm ]))
    texlive.combined.scheme-medium
    gnutls
    zstd # for undo-fu-session/undo-tree compression
  ];

  services.emacs.package = pkgs.emacs-unstable;
  services.emacs.enable = true;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  system.activationScripts =
    { # Installation script every time nixos-rebuild is run. So not during initial install.
      postActivation = {
        text = ''
          source ${config.system.build.setEnvironment}
          EMACS="/Users/anajulia/.config/emacs"
          if [ ! -d "$EMACS" ]; then
            ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git $EMACS
            yes | $EMACS/bin/doom install
           rm -r $XDG_CONFIG_HOME/doom
           ln -s ./ $XDG_CONFIG_HOME/doom
          fi
        ''; # It will always sync when rebuild is done. So changes will always be applied.
      };
    };

}
