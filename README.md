# dotfiles

My personal dotfiles for [NixOS](https://nixos.org). Keybindings are with [my keymap](https://git.sr.ht/~samhh/qmk_firmware/tree/samhh/item/keyboards/ergodox_ez/keymaps/samhh/README.md) in mind.

The repo configures two hosts, Alakazam and Tentacool. These are my personal machine and home server respectively.

## Usage

Build your desired host with `nixos-rebuild --flake`. Tentacool requires `--impure`. Example:

```console
# nixos-rebuild switch --flake ".#alakazam"
```

For now a few parts of the Alakazam config presume the repo will be at `~/dotfiles/`. Anything that couldn't be configured in Home Manager lives in `./cfg/`.

## History

I used to use Arch and macOS, as well as run a custom XMonad config. Those configs are preserved in this repo's commit history. Arch/XMonad specifically is available at `nixos~1`.
