#!/bin/bash

pacman -Qet > ~/docs/personal/logs/packages.txt
borgmatic
backblaze-b2 sync --delete --replaceNewer ~/.docs_repo/ b2://docs-borg

