{
  pkgs,
  mkDerivation,
  xmobar,
  aeson,
  alsa-core,
  alsa-mixer,
  async,
  base,
  bytestring,
  cairo,
  colour,
  containers,
  dbus,
  directory,
  extensible-exceptions,
  extra,
  filepath,
  hinotify,
  http-client-tls,
  http-conduit,
  http-types,
  iwlib,
  libmpd,
  mtl,
  old-locale,
  pango,
  parsec,
  parsec-numbers,
  process,
  regex-compat,
  stm,
  time,
  timezone-olson,
  timezone-series,
  transformers,
  unix,
  utf8-string,
  X11,
  X11-xft,
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
      aeson
      alsa-core
      alsa-mixer
      async
      base
      bytestring
      cairo
      colour
      containers
      dbus
      directory
      extensible-exceptions
      extra
      filepath
      hinotify
      http-client-tls
      http-conduit
      http-types
      iwlib
      libmpd
      mtl
      old-locale
      pango
      parsec
      parsec-numbers
      process
      regex-compat
      stm
      time
      timezone-olson
      timezone-series
      transformers
      unix
      utf8-string
      X11
      X11-xft
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
