#!/bin/sh

keyId=$(pass misc/backblaze.com/app-key-id-pass)
appKey=$(pass misc/backblaze.com/app-key-pass)

mkdir -p ~/logs/
pacman -Qet > ~/logs/packages.txt
tree /mnt/nas/tv/ > ~/logs/tv.txt
tree /mnt/nas/movies/ > ~/logs/movies.txt

duplicity --encrypt-key hello@samhh.com ~/keys/ "b2://$keyId:$appKey@keys-dup"
duplicity --encrypt-key hello@samhh.com ~/logs/ "b2://$keyId:$appKey@logs-dup"
duplicity --encrypt-key hello@samhh.com ~/vault/ "b2://$keyId:$appKey@vault-dup"

