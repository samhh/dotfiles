#!/bin/sh

echo "Preparing logs..."
mkdir -p /mnt/nas/logs/
"$(dirname "$0")"/seedbox/radarr.sh > /mnt/nas/logs/radarr.txt
