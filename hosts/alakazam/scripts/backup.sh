#!/bin/sh

keyId=$(pass misc/backblaze.com/app-key-id-pass)
appKey=$(pass misc/backblaze.com/app-key-pass)

echo "Preparing logs..."
mkdir -p /mnt/nas/logs/
tree /mnt/nas/tv/ > /mnt/nas/logs/tv.txt
tree /mnt/nas/movies/ > /mnt/nas/logs/movies.txt
"$(dirname "$0")"/seedbox/sonarr.sh > /mnt/nas/logs/sonarr.txt
"$(dirname "$0")"/seedbox/radarr.sh > /mnt/nas/logs/radarr.txt

echo "Preparing Tentacool service backups..."
ssh -t sam@tentacool \
  "
    rm -rf ~/backups/ &&
    mkdir -p ~/backups/ &&

    sudo podman volume export hass > ~/backups/starmie.tar &&
    tar -f ~/backups/starmie.tar --wildcards --delete 'home-assistant_v2.*' &&

    sudo podman exec pihole sh -c 'pihole -a -t && mv /pi-hole-tentacool-teleporter_*.tar.gz /backup.tar.gz' &&
    sudo podman cp pihole:/backup.tar.gz ~/backups/onix.tar.gz
  "
scp -r sam@tentacool:"~/backups/*" "/mnt/nas/archive/tentacool/"

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

echo "Backing up manuals..."
duplicity --encrypt-key hello@samhh.com /mnt/nas/manuals/ "b2://$keyId:$appKey@manuals-dup"

echo "Finished backups."
