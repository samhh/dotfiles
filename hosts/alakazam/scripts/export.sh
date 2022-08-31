#!/bin/sh

bm=/tmp/bookmarks.html

~/dotfiles/hosts/alakazam/scripts/flatmarks-html-build.sh > "$bm"

zip ~/exports.zip "$bm"
