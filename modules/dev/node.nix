# modules/dev/node.nix --- https://nodejs.org/en/

{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nodejs_latest
    yarn
  ];

  # Optionally, set user-specific environment variables
  # environment.variables.PATH = [ "${pkgs.yarn}/bin/yarn global bin" ];
}
