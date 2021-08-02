#!/bin/sh

rm -rf ~/.cabal/store/*/"$1"-*
cabal install --overwrite-policy=always
