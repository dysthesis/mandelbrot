{pkgs, ...}: {
  inherit
    (pkgs.haskellPackages)
    xmonad
    xmobar
    ;
}
