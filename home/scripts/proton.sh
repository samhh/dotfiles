#!/bin/bash

# See: https://gist.github.com/michaelbutler/f364276f4030c5f449252f2c4d960bd2

cd "/home/sam/.steam/steam/steamapps/compatdata/3552770541/pfx/"

export STEAM_COMPAT_DATA_PATH="/home/sam/.steam/steam/steamapps/compatdata/3552770541"
export WINEPREFIX="$PWD"

exec "/home/sam/.steam/steam/steamapps/common/Proton 5.0/proton" run $1

