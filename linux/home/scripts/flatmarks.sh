#!/usr/bin/env bash

base=~/.bookmarks/
cd $base

path=$(fzf)

# If we didn't select anything in fzf then bail
if [ -z "$path" ]; then
    exit 0
fi

url=$(head -1 "$base$path")

echo $url

