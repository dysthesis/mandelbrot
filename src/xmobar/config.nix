{pkgs, ...}: let
  inherit (pkgs) writeText;
in
  writeText "xmobar.hs"
  /*
  hs
  */
  ''
    import Xmobar (configFromArgs, xmobar)
    import Xmobar.Config (mkConfig)

    main :: IO ()
    main = cfg >>= xmobar
      where
       cfg = configFromArgs $ mkConfig "${./icons}"
  ''
