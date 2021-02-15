#!/bin/sh

inotifywait -m ~/ -e create --exclude "(\.steampid)|(\.steampath)" |
  while read dir action file; do
    notify-send "File created" "$dir$file"
  done

