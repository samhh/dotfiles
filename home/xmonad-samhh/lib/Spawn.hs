module Spawn (Spawn (..), toSpawnable) where

data Spawn
    = CloseNotif
    | CloseAllNotifs
    | DecVol
    | IncVol
    | DecVolMpv
    | IncVolMpv
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

toSpawnable :: Spawn -> String
toSpawnable CloseNotif = "dunstctl close"
toSpawnable CloseAllNotifs = "dunstctl close-all"
toSpawnable DecVol = "pactl set-sink-volume @DEFAULT_SINK@ -1%"
toSpawnable IncVol = "pactl set-sink-volume @DEFAULT_SINK@ +1%"
toSpawnable DecVolMpv = "playerctl -p mpv volume 0.05-"
toSpawnable IncVolMpv = "playerctl -p mpv volume 0.05+"
toSpawnable ToggleMuteOutput = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
toSpawnable ToggleMuteInput = "pactl set-source-mute '@DEFAULT_SOURCE@' toggle"
toSpawnable PlayPrevMpd = "playerctl previous -p mpd"
toSpawnable PauseMpd = "playerctl play-pause -p mpd"
toSpawnable PauseMpv = "playerctl play-pause -p mpv"
toSpawnable PlayNextMpd = "playerctl next -p mpd"
toSpawnable NewWallpaper = "systemctl --user start wallpaper"
-- Need to sleep to allow for keys to be released for scrot
toSpawnable TakeScreenshot = "sleep 0.1; scrot -s"
toSpawnable Apps = "~/scripts/apps.sh"
toSpawnable AllApps = "rofi -show run"
toSpawnable WebSearch = "~/scripts/web-search.sh"
toSpawnable Bookmarks = "~/scripts/flatmarks.sh"
toSpawnable WorkBookmarks = "~/scripts/flatmarks-work.sh"
toSpawnable Passwords = "~/scripts/passmenu.sh"
toSpawnable Usernames = "~/scripts/pass-prefixed-line.sh \"username: \" username"
toSpawnable Emails = "~/scripts/pass-prefixed-line.sh \"email: \" email"

