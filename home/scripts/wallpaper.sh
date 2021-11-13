#!/bin/sh

path=$(~/scripts/private/unsplash.sh)
h=$(date '+%H')

# pywal generates a "light" and "dark" Rofi theme, which is rather in contrast
# to the dynamism supported by the "-l" flag. We'll create our own symbolic
# link to keep it in sync after running pywal itself.
if [[ "$h" -gt 04 && "$h" -lt 17 ]]; then
  wal -l -i "$path"
  ln -sf ~/.cache/wal/colors-rofi-dark.rasi ~/.cache/wal/colors-rofi.rasi
else
  wal -i "$path"
  ln -sf ~/.cache/wal/colors-rofi-light.rasi ~/.cache/wal/colors-rofi.rasi
fi

# Reload dynamic configs
qutebrowser ':config-source'

