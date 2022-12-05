#!/bin/sh

echo "Preparing logs..."
mkdir -p /mnt/nas/logs/
"$(dirname "$0")"/seedbox/sonarr.sh > /mnt/nas/logs/sonarr.txt
"$(dirname "$0")"/seedbox/radarr.sh > /mnt/nas/logs/radarr.txt

echo "Preparing Tentacool service backups..."
ssh -t sam@tentacool \
  "
    rm -rf ~/backups/ &&
    mkdir -p ~/backups/ &&

    sudo tar -c -C /var/lib/zigbee2mqtt/ . > ~/backups/exeggutor.tar
    tar -f ~/backups/exeggutor.tar --delete ./log/ &&

    sudo podman volume export zwavejs2mqtt > ~/backups/sandshrew.tar

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
