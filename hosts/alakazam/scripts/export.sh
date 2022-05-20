#!/bin/sh

bm=/tmp/bookmarks.html
rss=/tmp/rss-feeds.opml

~/scripts/flatmarks-html-build.sh > "$bm"
newsboat -e > "$rss"

zip ~/exports.zip "$bm" "$rss"
