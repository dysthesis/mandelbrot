{
  mkDerivation,
  base,
  containers,
  lib,
  X11,
  xmonad,
  xmonad-contrib,
  srcDir,
}:
mkDerivation {
  pname = "xmonad";
  version = "1.0";
  src = "${srcDir}/xmonad";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    X11
    xmonad
    xmonad-contrib
  ];
  executableHaskellDepends = [base xmonad xmonad-contrib];
  description = "My XMonad setup";
  license = lib.licenses.gpl2Only;
  mainProgram = "xmonad";
}
