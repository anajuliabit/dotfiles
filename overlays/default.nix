self: super: {
    yabai = super.yabai.overrideAttrs (
    _: rec {
      version = "4.0.0-pre-91a42e7";
      src = builtins.fetchTarball {
        url = "https://github.com/koekeishiya/yabai/files/7915231/yabai-v4.0.0.tar.gz";
 #       sha256 = "RyuUEtv5HR/ZyKhpH81UzIx9CO4q/IMcPFUdtoVFAyI=";
      };

      installPhase = ''
        mkdir -p $out/bin
        mkdir -p $out/share/man/man1/
        cp ./bin/yabai $out/bin/yabai
        cp ./doc/yabai.1 $out/share/man/man1/yabai.1
      '';
    }
  );
}
