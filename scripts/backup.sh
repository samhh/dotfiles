#!/bin/sh

keyId=$(pass misc/backblaze.com/app-key-id-pass)
appKey=$(pass misc/backblaze.com/app-key-pass)

echo "Preparing logs..."
mkdir -p /mnt/nas/logs/
tree /mnt/nas/tv/ > /mnt/nas/logs/tv.txt
tree /mnt/nas/movies/ > /mnt/nas/logs/movies.txt
./seedbox/sonarr.sh > /mnt/nas/logs/sonarr.txt
./seedbox/radarr.sh > /mnt/nas/logs/radarr.txt

echo "Syncing mail..."
offlineimap
echo "Syncing contacts..."
vdirsyncer sync

echo "Backing up archive..."
duplicity --encrypt-key hello@samhh.com /mnt/nas/archive/ "b2://$keyId:$appKey@archive-dup"
echo "Backing up keys..."
duplicity --encrypt-key hello@samhh.com /mnt/nas/keys/ "b2://$keyId:$appKey@keys-dup"
echo "Backing up logs..."
duplicity --encrypt-key hello@samhh.com /mnt/nas/logs/ "b2://$keyId:$appKey@logs-dup"
echo "Backing up mail..."
duplicity --encrypt-key hello@samhh.com /mnt/nas/mail/ "b2://$keyId:$appKey@mail-dup"
echo "Finished backups."
