#!/bin/sh

inotifywait -m ~/ -e create --exclude "(\.steampid)|(\.steampath)|(\.Xauthority-c)|(\.Xauthority-l)|(\.b2_account_info-journal)" |
  while read dir action file; do
    notify-send "File created" "$dir$file"
  done

