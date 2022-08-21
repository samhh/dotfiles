#!/bin/sh

bm=/tmp/bookmarks.html

~/scripts/flatmarks-html-build.sh > "$bm"

zip ~/exports.zip "$bm"
