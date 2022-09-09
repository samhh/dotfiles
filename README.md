# dotfiles

Configuring the universe with [Nix](https://nixos.org).

The repo configures three hosts:

- Alakazam: Desktop, NixOS
- Lapras: Laptop, macOS w/ [nix-darwin](https://github.com/LnL7/nix-darwin)
- Tentacool: Homelab, NixOS

Keybindings are with [my keymap](https://git.sr.ht/~samhh/qmk_firmware/tree/samhh/item/keyboards/ergodox_ez/keymaps/samhh/README.md) in mind.

Lapras configures only the shell owing to [this issue](https://github.com/NixOS/nix/issues/956), and also because I mostly treat it more like a mobile device.

## Usage

Build your desired host with `nixos-rebuild --flake` (or `darwin-rebuild` on macOS). Example:

```console
# nixos-rebuild switch --flake ".#alakazam"
```

For now some Alakazam scripts presume the repo will be at `~/dotfiles/`. Anything that couldn't be configured in Home Manager lives in `./cfg/` (NB not `:/cfg/`, which defines custom Nix configuration).

### Packages

Some packages not in nixpkgs are defined in `:/pkgs/`. These can be built and run as flake outputs. Example:

```console
$ echo "declare const foo: Bar" | nix run ".#tshm" -- -a
foo :: Bar
```

Additionally some Home Manager modules are defined in `:/modules/`.

## History

I used to run various different operating systems and window managers. Those configs are preserved in this repo's commit history. Arch/XMonad specifically is available at `nixos~1`.
