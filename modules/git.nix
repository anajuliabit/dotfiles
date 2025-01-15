{ pkgs, lib, ... }:
{
  programs.git = {
    enable = true;
    userEmail = "anajuliabit@gmail.com";
    userName = "Ana Bittencourt";
    signing = {
      key = "AF665A6D96CA950A";
      signByDefault = true;
    };
    extraConfig = {
      gpg = {
        program = lib.mkForce "${pkgs.gnupg}/bin/gpg2";
      };
      commit.gpgsign = true;
    };
  };
}
