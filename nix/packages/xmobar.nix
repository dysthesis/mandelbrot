{
  pkgs,
  mkDerivation,
  extra,
  process,
  xmobar,
  async,
  base,
  containers,
  directory,
  filepath,
  parsec,
  unix,
  X11,
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
      async
      xmobar
      base
      containers
      directory
      filepath
      parsec
      unix
      X11
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
