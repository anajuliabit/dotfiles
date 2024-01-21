{ config, inputs, pkgs, lib, ... }:
let
  hm-config = import ../modules/home-manager.nix {
    config = config;
    pkgs = pkgs;
    lib = lib;
    inputs = inputs;
  };
in {
  imports = [
    <home-manager/nix-darwin>
  ];
  # Homebrew is used to install impure software only (Mac Apps)
  home-manager = {
    useGlobalPkgs = true;
    users.anajulia = { pkgs, lib, ... }:
      lib.recursiveUpdate hm-config {
        programs = {
          rtx.enable = true;
          command-not-found.enable = true;
        };
        home.enableNixpkgsReleaseCheck = false;
        home.packages = pkgs.callPackage ./packages.nix { };
        home.stateVersion = "22.11";
        home.sessionVariables = {
          XDG_CACHE_HOME = "\${HOME}/.cache";
          XDG_CONFIG_HOME = "\${HOME}/.config";
          XDG_BIN_HOME = "\${HOME}/.local/bin";
          XDG_DATA_HOME = "\${HOME}/.local/share";
          EDITOR = "emacs";
          PATH =
            "/Users/$USER/.config/emacs/bin:/Users/$USER/Library/Python/3.9/bin:/etc/profiles/per-user/$USER/bin:/run/current-system/sw/bin/:$HOME/.local/bin:/Users/anajulia/.local/share/rtx/installs/node/18.19.0/bin:$PATH";
          LIBRARY_PATH = "${
              lib.makeLibraryPath [ pkgs.libiconv ]
            }\${LIBRARY_PATH:+:$LIBRARY_PATH}";
        };
        disabledModules = [ "targets/darwin/linkapps.nix" ];
        home.activation.copyApplications = let
          apps = pkgs.buildEnv {
            name = "home-manager-applications";
            paths = pkgs.callPackage ./packages.nix { };
            pathsToLink = "/Applications";
          };
        in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          baseDir="$HOME/Applications/Home Manager Apps"
          if [ -d "$baseDir" ]; then
            rm -rf "$baseDir"
          fi
          mkdir -p "$baseDir"
          for appFile in ${apps}/Applications/*; do
            target="$baseDir/$(basename "$appFile")"
            $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
            $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
          done
        '';
      };
  };
}
