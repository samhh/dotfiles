# dotfiles

My personal dotfiles targeting a machine running [xmonad](https://xmonad.org) on [Arch](https://archlinux.org). Colour schemes are generated on the fly with [pywal](https://github.com/dylanaraps/pywal), either light or dark [depending upon the time of day](https://github.com/samhh/dotfiles/blob/master/home/scripts/wallpaper.sh). Keybindings are with [my keymap](https://git.sr.ht/~samhh/qmk_firmware/tree/samhh/item/keyboards/ergodox_ez/keymaps/samhh/README.md) in mind.

## Usage

A shell script (`./link.sh`) is included that sets up symlinks for everything. This uses [Stow](https://www.gnu.org/software/stow/) under the hood. A Nix shell config is included.

Most software is managed declaratively with [Home Manager](https://github.com/nix-community/home-manager) ([Nix](https://nixos.org)). What's left should be captured by the [pacmanfile](https://github.com/cloudlena/pacmanfile) config.

