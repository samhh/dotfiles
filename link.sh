#!/bin/sh

# Set PWD to location of script (i.e. dotfiles repo root)
cd "$(dirname "$0")"

stow -R root -t /
stow -R home -t $HOME

