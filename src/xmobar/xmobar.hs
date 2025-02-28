import Xmobar

config :: Config
config =
  defaultConfig
    { font = "JBMono Nerd Font 9",
      allDesktops = True,
      bgColor = "#000000",
      fgColor = "#ffffff",
      persistent = True,
      iconRoot = ".config/xmobar/icons",
      iconOffset = -1,
      position = BottomH 34,
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
          Run $ Memory ["-t", "\xf0193 <usedratio>%"] 10,
          Run $ DiskU [("/", "\xf0a0 <used>/<size>")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 10,
          Run $ Cpu ["-t", "\xf4bc <total>%"] 10,
          Run $ Date "%a %_d %b, %H:%M" "date" 10,
          Run $ Swap ["-t", "\xf0bcd <usedratio>%"] 10,
          Run $ Com "taskwarrior rc.verbose: rc.report.next.columns:due.relative rc.report.next.labels:1 limit:1 next" [] "task" 30,
          Run $
            DynNetwork
              [ "--template",
                "\xf102 <tx>kB/s \xf103 <rx>kB/s",
                "--Low",
                "102400", -- units: B/s
                "--High",
                "10240000", -- units: B/s
                "--low",
                "#fab387",
                "--normal",
                "#fab387",
                "--high",
                "#fab387"
              ]
              10
        ],
      template = " %XMonadLog% } \xf133  %calendar%  <fc=#6c7086>\xf01d9</fc>  %date%  <fc=#6c7086>\xf01d9</fc>  \xf0ae  %task% { <fc=#cba6f7>%cpu%</fc>  <fc=#89b4fa>%memory%</fc>  <fc=#94e2d5>%disku%</fc>  <fc=#a6e3a1>%swap%</fc>  <fc=#fab387>%dynnetwork%</fc>  <fc=#6c7086>\xf01d9</fc> %YSSY%   ",
      alignSep = "}{"
    }

main :: IO ()
main = xmobar config -- or: configFromArgs config >>= xmobar
