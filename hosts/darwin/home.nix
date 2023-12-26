{ config, inputs, pkgs, lib, ... }:
let
  hm-config = import ../../modules/home-manager.nix { config = config; pkgs = pkgs; lib = lib; };
  home-manager = builtins.fetchTarball
    "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in {
  imports = [ "${home-manager}/nix-darwin" ];
  home-manager = {
    useGlobalPkgs = true;
    users.anajulia = { pkgs, lib, ... }: lib.recursiveUpdate hm-config {
      programs = {
        rtx.enable = true;
        command-not-found.enable = true;
      };
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix {};
      home.stateVersion = "22.11";
      home.sessionVariables = {
        XDG_CACHE_HOME = "\${HOME}/.cache";
        XDG_CONFIG_HOME = "\${HOME}/.config";
        XDG_BIN_HOME = "\${HOME}/.local/bin";
        XDG_DATA_HOME = "\${HOME}/.local/share";
        EDITOR = "emacs";
        PATH =
          "/Users/anajulia/.config/emacs/bin:/Users/$USER/Library/Python/3.9/bin:$PATH";
        #CXX = "clang++";
        LIBRARY_PATH = "${
          lib.makeLibraryPath [ pkgs.libiconv ]
        }\${LIBRARY_PATH:+:$LIBRARY_PATH}";
      };
      disabledModules = [ "targets/darwin/linkapps.nix" ];
      home.activation.copyApplications = let
          apps = pkgs.buildEnv {
            name = "home-manager-applications";
            paths = pkgs.callPackage ./packages.nix {};
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

    #  home.sessionVariables = rec {

    #  };

    #  PATH = [
    #    "\${HOME}/.bin"
    #    "\${XDG_BIN_HOME}"
    #    "\${HOME}/.node_modules"
    #  ];

        #   modules.emacs.enable = true;
    };
  };
}
