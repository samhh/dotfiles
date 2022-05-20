#!/bin/sh

key="B7C77CD313EAE76366FE1D7E4667250BD56735A8"

# These'll be separated at newlines, hence the awkward formatting.
servers="keys.openpgp.org
pgp.mit.edu
pool.sks-keyservers.net
keyserver.ubuntu.com"

echo "$servers" | tr ' ' '\n' | while read -r server; do
  gpg --keyserver "$server" --send-keys "$key"
done
