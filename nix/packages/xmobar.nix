{
  mkDerivation,
  base,
  extra,
  process,
  xmobar,
  srcDir,
}:
mkDerivation {
  pname = "xmobar";
  version = "1.0";
  src = "${srcDir}/xmobar";
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [base extra process xmobar];
  executableHaskellDepends = [base xmobar];
  doHaddock = false;
  description = "My XMobar config";
  license = "unknown";
  mainProgram = "xmobar";
}
