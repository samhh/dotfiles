module Spawn (Spawn (..), toSpawnable, BrowserProfile (..), profileInstanceName) where

data Spawn
    = CloseNotif
    | CloseAllNotifs
    | DecVol
    | IncVol
    | ToggleMuteOutput
    | ToggleMuteInput
    | PlayPrevMpd
    | PlayNextMpd
    | PauseMpd
    | PauseMpv
    | NewWallpaper
    | TakeScreenshot
    | Apps
    | AllApps
    | WebSearch
    | Bookmarks
    | WorkBookmarks
    | Passwords
    | Usernames
    | Emails
    | LatencyCheck
    | DefinitionLookup

toSpawnable :: Spawn -> String
toSpawnable CloseNotif = "dunstctl close"
toSpawnable CloseAllNotifs = "dunstctl close-all"
toSpawnable DecVol = "pactl set-sink-volume @DEFAULT_SINK@ -1%"
toSpawnable IncVol = "pactl set-sink-volume @DEFAULT_SINK@ +1%"
toSpawnable ToggleMuteOutput = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
toSpawnable ToggleMuteInput = "pactl set-source-mute '@DEFAULT_SOURCE@' toggle"
toSpawnable PlayPrevMpd = "playerctl previous -p mpd"
toSpawnable PauseMpd = "playerctl play-pause -p mpd"
toSpawnable PauseMpv = "playerctl play-pause -p mpv"
toSpawnable PlayNextMpd = "playerctl next -p mpd"
toSpawnable NewWallpaper = "~/scripts/wallpaper.sh"
-- Need to sleep (-s) to allow for keys to be released for scrot.
-- Need to freeze (-f) to prevent scrot capturing the selection rectangle.
toSpawnable TakeScreenshot = "sleep 0.1; scrot -s -f"
toSpawnable Apps = "~/scripts/apps.sh"
toSpawnable AllApps = "rofi -show run"
toSpawnable WebSearch = "~/scripts/web-search.sh"
toSpawnable Bookmarks = "~/scripts/flatmarks.sh"
toSpawnable WorkBookmarks = "~/scripts/flatmarks-work.sh"
toSpawnable Passwords = "~/scripts/passmenu.sh"
toSpawnable Usernames = "~/scripts/pass-prefixed-line.sh \"username: \" username"
toSpawnable Emails = "~/scripts/pass-prefixed-line.sh \"email: \" email"
toSpawnable LatencyCheck = "alacritty -e ping 1.1.1.1"
toSpawnable DefinitionLookup = "qutebrowser $(xclip -o -selection primary; echo -n '!d')"

data BrowserProfile
  = Personal
  | Work

profileInstanceName :: BrowserProfile -> String
profileInstanceName Personal = "qutebrowser"
profileInstanceName Work     = "unsplash"
