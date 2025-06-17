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
      pull.rebase = false;
      init.defaultBranch = "main";

    };

    ignores = [
      ".DS_STORE"
      ".DS_Store"
      "*~"
      "*.bak"
      "*.log"
      "*.swp"
      "node_modules"
      ".direnv/"
      ".devenv/"
    ];
  };

   programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
    };
  };
}
