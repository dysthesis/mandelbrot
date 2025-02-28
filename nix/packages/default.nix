{pkgs, ...}: rec {
  xmonad = let
    inherit (pkgs) haskellPackages;
    inherit (builtins) readFile;
    config = readFile ../../xmonad.hs;

    ghcWithPackages = haskellPackages.ghcWithPackages;
    packages = self:
      with self; [
        xmonad-contrib
        xmonad-extras
        xmobar
      ];
    xmonadAndPackages = self: [self.xmonad] ++ packages self;
    xmonadEnv = ghcWithPackages xmonadAndPackages;
    libDir = ./../../lib;

    configured =
      pkgs.writers.writeHaskellBin "xmonad" {
        ghc = haskellPackages.ghc;
        libraries = xmonadAndPackages haskellPackages;
        ghcArgs = [
          "-hidir /tmp" # place interface files in /tmp, otherwise ghc tries to write them to the nix store
          "-odir /tmp" # place object files in /tmp, otherwise ghc tries to write them to the nix store
          # Add the local library directory to GHC's include path
          "-i${libDir}"
        ];
      }
      config;
  in
    pkgs.runCommandLocal "xmonad" {
      nativeBuildInputs = [pkgs.makeWrapper];
    } (
      /*
      sh
      */
      ''
        install -D ${xmonadEnv}/share/man/man1/xmonad.1.gz $out/share/man/man1/xmonad.1.gz
        makeWrapper ${configured}/bin/xmonad $out/bin/xmonad \
      ''
      + ''
        --set XMONAD_XMESSAGE "${pkgs.xorg.xmessage}/bin/xmessage"
      ''
    );
  default = xmonad;
}
