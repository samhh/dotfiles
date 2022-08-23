# dotfiles

My personal dotfiles for [NixOS](https://nixos.org). Keybindings are with [my keymap](https://git.sr.ht/~samhh/qmk_firmware/tree/samhh/item/keyboards/ergodox_ez/keymaps/samhh/README.md) in mind.

The repo configures three hosts: Alakazam, Lapras and Tentacool. These are my desktop, laptop, and home server respectively. Alakazam and Tentacool run NixOS whilst Lapras runs Nix on macOS.

Lapras configures only the shell owing to [this issue](https://github.com/NixOS/nix/issues/956), and also because I mostly treat it more like a mobile device.

## Usage

Build your desired host with `nixos-rebuild --flake`. Example:

```console
# nixos-rebuild switch --flake ".#alakazam"
```

For now a few parts of the Alakazam config presume the repo will be at `~/dotfiles/`. Anything that couldn't be configured in Home Manager lives in `./cfg/`.

## History

I used to run various different operating systems and window managers. Those configs are preserved in this repo's commit history. Arch/XMonad specifically is available at `nixos~1`.
