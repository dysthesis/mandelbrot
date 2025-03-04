{
  pkgs,
  mkDerivation,
  extra,
  process,
  xmobar,
  base,
  srcDir,
}: let
  configFile = import "${srcDir}/xmobar/config.nix" {inherit pkgs;};
in
  mkDerivation {
    pname = "xmobar";
    version = "1.0";
    src = "${srcDir}/xmobar";
    isLibrary = false;
    isExecutable = true;
    libraryHaskellDepends = [
      base
      extra
      process
      xmobar
    ];
    executableHaskellDepends = [
      base
      extra
      process
      xmobar
    ];
    doHaddock = false;
    description = "My XMobar config";
    license = "unknown";
    mainProgram = "xmobar";

    postPatch =
      /*
      bash
      */
      ''
        cp ${configFile} xmobar.hs
      '';
  }
