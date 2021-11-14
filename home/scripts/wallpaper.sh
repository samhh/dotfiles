#!/bin/sh

cache_path=~/.cache/wal/
img_path=$(~/scripts/private/unsplash.sh)
h=$(date '+%H')

# pywal generates a "light" and "dark" Rofi theme, which is rather in contrast
# to the dynamism supported by the "-l" flag. We'll create our own symbolic
# link to keep it in sync after running pywal itself.
# We'll also store whether the theme is light or dark in our own file since
# pywal's templates don't indicate this, and it's cheaper than testing the
# colours ourselves.
if [[ "$h" -gt 04 && "$h" -lt 17 ]]; then
  echo light > "$cache_path/_theme"
  wal -l -i "$img_path"
  ln -sf "$cache_path/colors-rofi-dark.rasi" "$cache_path/colors-rofi.rasi"
else
  echo dark > "$cache_path/_theme"
  wal -i "$img_path"
  ln -sf "$cache_path/colors-rofi-light.rasi" "$cache_path/colors-rofi.rasi"
fi

# Source qutebrowser's config only if there's an active instance, else it will
# spawn one.
if pgrep qutebrowser &> /dev/null; then
  qutebrowser ':config-source'
fi

