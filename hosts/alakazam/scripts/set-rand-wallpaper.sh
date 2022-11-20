#!/bin/sh

dir="$1"

# The not path hides dotfiles and `@eaDir`.
find "$dir" -type f -not -path '*/[@.]*' | shuf -n1 | xargs swaybg -m fill -i
