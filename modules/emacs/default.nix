{ config, pkgs, inputs, ... }:

{

  services.emacs.package = pkgs.emacs-unstable;
  services.emacs.enable = true;
  user.  home-manager.users.anajulia = {
    home.file = {
      ".emacs.d/init.el" = {
        source = ./init.el;
        recursive = true;
      };
    };
  };
}
