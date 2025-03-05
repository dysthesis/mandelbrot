pkgs:
pkgs.mkShell {
  name = "Poincare";
  packages = with pkgs;
    [
      nixd
      alejandra
      statix
      deadnix
      haskell-language-server
      fourmolu
      ghc
    ]
    ++ (with pkgs.haskellPackages; [
      base
      X11
      xmonad
      xmobar
      xmonad-contrib
      process
      text
      extra
    ]);
}
