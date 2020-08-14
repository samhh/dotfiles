#!/bin/sh

# Set PWD to location of script (i.e. dotfiles repo root)
cd "$(dirname "$0")"

stow -R --adopt root -t /
stow -R --adopt home -t $HOME

