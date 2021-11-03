module Main where

import           Color  (Colorscheme (color0, color4), getColorscheme, hideous)
import           Xmobar (CommandReader (CommandReader), Config (..),
                         Date (Date), Monitors (..), Runnable (Run),
                         StdinReader (StdinReader), XPosition (Top),
                         defaultConfig, xmobar)

main :: IO ()
main = xmobar . cfg =<< getColorscheme

cfg :: Maybe Colorscheme -> Config
cfg cs =
  defaultConfig
    { font = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true",
      bgColor = maybe hideous color0 cs,
      fgColor = maybe hideous color4 cs,
      position = Top,
      sepChar = "%",
      alignSep = "}{",
      template = " %StdinReader% | %cpu%, %cputemps% | %memory% | %dynnetwork% }{ %mpris2%%mpd% | %default:Master% | %default:Capture% | rss: %rss% | %date% ",
      commands =
        [ Run StdinReader
        , Run $ CommandReader "~/scripts/poll-script.sh ~/scripts/cpu-temps.sh 2s" "cputemps"
        , Run $ CommandReader "~/scripts/poll-script.sh ~/scripts/rss-unread.sh 2h" "rss"
        , Run $ DynNetwork
            [ "--template"
            , "net: <tx> kB/s up, <rx> kB/s down"
            , "--Low"
            , "50000"
            , "--High"
            , "1000000"
            , "--low"
            , "#d8dee9"
            , "--normal"
            , "#d8dee9"
            , "--high"
            , "#ebcb8b"
            ]
            10
        , Run $ Cpu
            [ "--template"
            , "cpu: <total>%"
            , "--Low"
            , "50"
            , "--High"
            , "85"
            , "--low"
            , "#d8dee9"
            , "--normal"
            , "#d8dee9"
            , "--high"
            , "#ebcb8b"
            ]
            10
        , Run $ Memory
            [ "--template"
            , "mem: <usedratio>%"
            , "--Low"
            , "10"
            , "--High"
            , "75"
            , "--low"
            , "#d8dee9"
            , "--normal"
            , "#d8dee9"
            , "--high"
            , "#ebcb8b"
            ]
            10
        , Run $ Volume
            "default"
            "Capture"
            [ "--template"
            , "in: <volume>% <status>"
            , "--"
            , "-C"
            , "#ebcb8b"
            , "-c"
            , "#d8dee9"
            ]
            10
        , Run $ Volume
            "default"
            "Master"
            [ "--template"
            , "out: <volume>% <status>"
            , "--"
            , "-C"
            , "#d8dee9"
            , "-c"
            , "#ebcb8b"
            ]
            10
        , Run $ Mpris2
            "mpv"
            [ "--template"
            , "<title> / "
            , "--nastring"
            , ""
            ]
            10
        , Run $ MPD
            [ "--template"
            , "<fc=#81a1c1><state></fc>: <artist> - <title> (<volume>%)"
            ]
            10
        , Run $ Date "<fc=#e5e9f0>%H:%M, %d/%m</fc>" "date" 10
        ]
    }
