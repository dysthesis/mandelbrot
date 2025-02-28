{pkgs, ...}: let
  inherit (pkgs.haskellPackages) callPackage;
  inherit
    (pkgs.haskell.lib)
    buildFromSdist
    overrideCabal
    ;
  srcDir = ../../src;
  xmonad = buildFromSdist (callPackage ./xmonad.nix {inherit srcDir;});
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
      overrideCabal (callPackage ./xmobar.nix {inherit srcDir;})
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
          pkgs.makeWrapper
        ];
      installPhase =
        oa.installPhase
        + ''
          wrapProgram $out/bin/xmobar \
            --prefix FONTCONFIG_FILE : ${pkgs.makeFontsConf {fontDirectories = [pkgs.nerd-fonts.jetbrains-mono];}}
        '';
    });
in {
  xmonad = addFontConfig xmonad;
  xmobar = addFontConfig xmobar;
}
