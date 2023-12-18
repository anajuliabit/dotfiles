{ pkgs, ... }:

{
  services.emacs.package = pkgs.emacs-unstable;
}
