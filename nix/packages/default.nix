{
  inputs,
  pkgs,
  ...
}: let
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
  addFontConfig = drv: name:
    drv.overrideAttrs (old: {
      buildInputs =
        old.buildInputs
        or []
        ++ [
          pkgs.makeWrapper
        ];
      installPhase =
        old.installPhase
        + ''
          wrapProgram $out/bin/${name}-configured \
            --prefix FONTCONFIG_FILE : ${pkgs.makeFontsConf {fontDirectories = [inputs.babel.packages.${pkgs.system}.jbcustom-nf];}}
        '';
    });
in {
  xmonad = addFontConfig xmonad "xmonad";
  xmobar = addFontConfig xmobar "xmobar";
}
