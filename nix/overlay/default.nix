{}: final: prev:
with final.haskell.lib;
with final.lib; let
  srcDir = ../../src;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
      self: _super: let
        xmonad = buildFromSdist (self.callPackage ./xmonad.nix {inherit srcDir;});
        xmobar = let
          ghcOptions = [
            "O2"
            "Wall"
          ];
          mkGhcOptions = map (opt: "--ghc-options=-${opt}");
          fWith = [
            "alsa"
            "conduit"
            "datezone"
            "dbus"
            "inotify"
            "iwlib"
            "mpd"
            "mpris"
            "rtsopts"
            "threaded"
            "utf8"
            "uvmeter"
            "weather"
            "xft"
            "xpm"
          ];
          mkFWith = map (opt: "-fwith_${opt}");
        in
          buildFromSdist (
            overrideCabal (self.callPackage ./xmobar.nix {inherit srcDir;})
            (old: {
              configureFlags =
                (old.configureFlags or [])
                ++ mkGhcOptions ghcOptions
                ++ mkFWith fWith;
            })
          );
        addFontConfig = drv:
          drv.overrideAttrs (oa: {
            buildInputs =
              oa.buildInputs
              or []
              ++ [
                final.makeWrapper
              ];
            installPhase =
              oa.installPhase
              + ''
                wrapProgram $out/bin/xmobar-app \
                  --prefix FONTCONFIG_FILE : ${final.makeFontsConf {fontDirectories = [final.nerd-fonts.jetbrains-mono];}}
              '';
          });
      in {
        xmonad = addFontConfig xmonad;
        xmobar = addFontConfig xmobar;
      }
    );
  });
in {
  inherit haskellPackages;
}
