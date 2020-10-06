#!/bin/bash

pacman -Qet > ~/docs/logs/packages.txt
tree /mnt/nas/tv/ > ~/docs/logs/tv.txt
tree /mnt/nas/movies/ > ~/docs/logs/movies.txt
borgmatic
backblaze-b2 sync --delete --replaceNewer ~/.docs_repo/ b2://docs-borg

