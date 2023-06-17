{ config, lib, pkgs, inputs, ... }:

with lib;

let cfg = config.modules.emacs;
in {
  options.modules.emacs = {
    enable = mkOption {
        default = true;
        type = types.bool;
    };
    doom = rec {
      enable = mkOption {
        default = true;
        type = types.bool;
      };
      repoUrl = mkOption {
        default = "https://github.com/hlissner/doom-emacs.git";
        type = types.str;
      };
      configPath = mkOption {
        default = "./doom";
        type = types.str;
      };
    };
  };

  config = mkIf cfg.enable {
   # nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    home.packages = with pkgs; [
      ## Emacs itself
      #binutils       # native-comp needs 'as', provided by this
      # 28.2 + native-comp
     # ((emacsPackagesFor emacsNativeComp).emacsWithPackages
     #   (epkgs: [ epkgs.vterm ]))
      ## Doom dependencies
      gnutls              # for TLS connectivity
      ## Optional dependencies
      imagemagick         # for image-dired
      #(mkIf (config.programs.gnupg.agent.enable)
      #  pinentry_emacs)   # in-emacs gnupg prompts
      zstd                # for undo-fu-session/undo-tree compression

      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      ## Module dependencies
      # :checkers spell
      #(aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :tools editorconfig
      #editorconfig-core-c # per-project style config
      # :tools lookup & :lang org +roam
      #sqlite
      # :lang latex & :lang org (latex previews)
      #texlive.combined.scheme-medium
      # :lang beancount
      #beancount
      #unstable.fava  # HACK Momentarily broken on nixos-unstable
    ];

    #env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    #modules.shell.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    #fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

   # system.userActivationScripts = mkIf cfg.doom.enable {
   #   installDoomEmacs = ''
   #     if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
   #        git clone --depth=1 --single-branch "${cfg.doom.repoUrl}" "$XDG_CONFIG_HOME/emacs"
   #        rm -r $XDG_CONFIG_HOME/doom
   #        ln -s "${cfg.doom.configPath}" "$XDG_CONFIG_HOME/doom"
   #     fi
   #   '';
   # };
    programs.emacs.enable = true;
  };
}
