{
  pkgs,
  mkDerivation,
  base,
  extra,
  process,
  xmobar,
  text,
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
    libraryHaskellDepends = [base extra process xmobar text];
    executableHaskellDepends = [base xmobar];
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
