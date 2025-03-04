{pkgs, ...}: let
  inherit (pkgs) writeText;
in
  writeText "xmobar.hs"
  /*
  hs
  */
  ''
    import Xmobar

    config :: Config
    config =
      defaultConfig
        { font = "JBMono Nerd Font 10",
          bgColor = "#000000",
          fgColor = "#ffffff",
          persistent = True,
    	allDesktops = true,
          iconRoot = "${./icons}",
          iconOffset = -1,
          position = BottomH 32,
          commands =
            [ Run XMonadLog,
              Run $
                WeatherX
                  "YSSY"
                  [ ("clear", "\xf522"),
                    ("sunny", "\xf522"),
                    ("mostly clear", "\xe21d"),
                    ("mostly sunny", "\xe21d"),
                    ("partly sunny", "\xe21d"),
                    ("fair", "🌑"),
                    ("cloudy", "\xf0590"),
                    ("overcast", "\xe30c"),
                    ("partly cloudy", "\xf0595"),
                    ("mostly cloudy", "\xf0595"),
                    ("considerable cloudiness", "\xf0595")
                  ]
                  ["-t", "<skyConditionS> <tempC>°C"]
                  18000,
              Run $ Memory ["-t", "<usedratio>%"] 10,
              Run $ DiskU [("/", "\xf0a0 <used>/<size>")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 10,
              Run $ Cpu ["-t", "<total>%"] 10,
              Run $ Date "%a, %_d %b, %H:%M" "date" 10,
              Run $ Swap ["-t", "<usedratio>%"] 10,
              Run $ Com "taskwarrior rc.verbose: rc.report.next.columns:due.relative rc.report.next.labels:1 limit:1 next" [] "task" 30,
              Run $
                DynNetwork
                  [ "--template",
                    "\xf102 <tx><fc=#ffffff>kB/s</fc> \xf103 <rx><fc=#ffffff>kB/s</fc>",
                    "--Low",
                    "102400", -- units: B/s
                    "--High",
                    "10240000", -- units: B/s
                    "--low",
                    "#ffffff",
                    "--normal",
                    "#ffffff",
                    "--high",
                    "#ffffff"
                  ]
                  10
            ],
          template = " %XMonadLog% } \xf133  %date%  { <fc=#789978>\xf4bc</fc> %cpu%  <fc=#708090>\xf0193</fc> %memory%  <fc=#FFAA88>\xf0bcd</fc> %swap%  <fc=#7788AA>%dynnetwork%</fc>  <fc=#6c7086>\xf01d9</fc> %YSSY%   ",
          alignSep = "}{"
        }

    main :: IO ()
    main = xmobar config -- or: configFromArgs config >>= xmobar
  ''
