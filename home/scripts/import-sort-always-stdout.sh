#!/usr/bin/sh

path="$1"
out=$(./node_modules/.bin/import-sort "$path")

if [[ ! -z $out ]]; then
  echo "$out"
else
  cat "$path"
fi
