#!/usr/bin/env bash

base=${1:-~/bookmarks/}

# Find everything in bookmarks dir not a directory and not matching git pattern
readarray -d '' paths < <(find "$base" -type f -not -path '*/.git*' -print0)

# Build up a markdown file from our files (DIRS/HEADINGS TODO)
md=
for path in "${paths[@]}"; do
  # Remove base path prefix
  path="${path#"$base"}"

  md+="[$path]"
  md+="($(head -1 "$base$path"))"
  md+=$'\n'
done

# Convert markdown to HTML and print to stdout
echo "$md" | pandoc -s --quiet
