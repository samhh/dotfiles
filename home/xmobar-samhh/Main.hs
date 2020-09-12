module Main where

import Xmobar

main :: IO ()
main = xmobar cfg

cfg :: Config
cfg =
  defaultConfig
    { font = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true",
      bgColor = "#2e3440",
      fgColor = "#d8dee9",
      position = Top,
      sepChar = "%", -- delineator between plugin names and straight text
      alignSep = "}{", -- separator between left-right alignment
      template = " %StdinReader% | %cpu% | %memory% | %dynnetwork% }{ %mpris2%%mpd% | %alsa:default:Master% | %date% ",
      lowerOnStart = True, -- send to bottom of window stack on start
      hideOnStart = False, -- start with window unmapped (hidden)
      allDesktops = True, -- show on all desktops
      overrideRedirect = True, -- set the Override Redirect flag (Xlib)
      pickBroadest = False, -- choose widest display (multi-monitor)
      persistent = True, -- enable/disable hiding (True = disabled)
      commands =
        [ Run $ StdinReader,
          Run $
            DynNetwork
              [ "--template",
                "<dev>: <tx> kB/s up, <rx> kB/s down",
                "--Low",
                "50000", -- units: B/s
                "--High",
                "1000000", -- units: B/s
                "--low",
                "#d8dee9",
                "--normal",
                "#d8dee9",
                "--high",
                "#ebcb8b"
              ]
              10,
          Run $
            Cpu
              [ "--template",
                "cpu: <total>%",
                "--Low",
                "50", -- units: %
                "--High",
                "85", -- units: %
                "--low",
                "#d8dee9",
                "--normal",
                "#d8dee9",
                "--high",
                "#ebcb8b"
              ]
              10,
          -- , Run $ CoreTemp       [ "--template" , "temp: <core0>°C|<core1>°C"
          --                      , "--Low"      , "40"
          --                      , "--High"     , "75"
          --                      , "--low"      , "#d8dee9"
          --                      , "--normal"   , "#d8dee9"
          --                      , "--high"     , "#ebcb8b"
          --                      ] 50

          -- , Run $ CommandReader  "~/scripts/cpu-temps.sh" "cputemps"

          Run $
            Memory
              [ "--template",
                "mem: <usedratio>%",
                "--Low",
                "10",
                "--High",
                "75",
                "--low",
                "#d8dee9",
                "--normal",
                "#d8dee9",
                "--high",
                "#ebcb8b"
              ]
              10,
          Run $
            Alsa
              "default"
              "Master"
              [ "--template",
                "Out: <dB>dB <status>",
                "--",
                "--onc",
                "#d8dee9",
                "--offc",
                "#ebcb8b"
              ],
          Run $
            Alsa
              "pcm.usb"
              "Mic"
              [ "--template",
                "In: <volumestatus> <status> <volumeipat> (TODO)",
                "--",
                "--onc",
                "#d08770",
                "--offc",
                "#d8dee9"
              ],
          Run $
            Mpris2
              "mpv"
              [ "--template",
                "<title> / ",
                "--nastring",
                ""
              ]
              10,
          Run $
            MPD
              [ "--template",
                "<fc=#81a1c1><state></fc>: <artist> - <title> (<volume>%)"
              ]
              10,
          Run $ Date "<fc=#e5e9f0>%H:%M, %d/%m</fc>" "date" 10
        ]
    }
