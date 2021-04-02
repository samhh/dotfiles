#!/bin/sh

keyId=$(pass misc/backblaze.com/app-key-id-pass)
appKey=$(pass misc/backblaze.com/app-key-pass)

mkdir -p /mnt/nas/logs/
pacman -Qet > /mnt/nas/logs/packages.txt
tree /mnt/nas/tv/ > /mnt/nas/logs/tv.txt
tree /mnt/nas/movies/ > /mnt/nas/logs/movies.txt

offlineimap

duplicity --encrypt-key hello@samhh.com /mnt/nas/keys/ "b2://$keyId:$appKey@keys-dup"
duplicity --encrypt-key hello@samhh.com /mnt/nas/logs/ "b2://$keyId:$appKey@logs-dup"
duplicity --encrypt-key hello@samhh.com /mnt/nas/mail/ "b2://$keyId:$appKey@mail-dup"
duplicity --encrypt-key hello@samhh.com ~/vault/ "b2://$keyId:$appKey@vault-dup"

