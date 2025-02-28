{
  mkDerivation,
  base,
  process,
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
    X11
    xmonad
    xmonad-contrib
    process
  ];
  executableHaskellDepends = [base xmonad xmonad-contrib];
  description = "My XMonad setup";
  license = "unknown";
  mainProgram = "xmonad";
}
