#!/bin/sh

key="B7C77CD313EAE76366FE1D7E4667250BD56735A8"

read -r -d '' servers << fin
keys.openpgp.org
pgp.mit.edu
pool.sks-keyservers.net
keyserver.ubuntu.com
fin

echo "$servers" | tr ' ' '\n' | while read server; do
  gpg --keyserver "$server" --send-keys "$key"
done
