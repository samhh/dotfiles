# dotfiles

Configuring the universe with [Nix](https://nixos.org).

The repo configures two hosts:

- Alakazam: Desktop, NixOS
- Tentacool: Homelab, NixOS

Keybindings are with [my keymap](https://git.sr.ht/~samhh/qmk_firmware/tree/samhh/item/keyboards/ergodox_ez/keymaps/samhh/README.md) in mind.

## Usage

Build your desired host with `nixos-rebuild --flake`. Additionally Alakazam can deploy to Tentacool.

```console
# nixos-rebuild switch --flake ".#alakazam"
$ deploy ".#tentacool"
```

For now some Alakazam scripts presume the repo will be at `~/dotfiles/`.

### Packages

Some packages not in nixpkgs are defined in `:/packages/`. These can be built and run as flake outputs.

```console
$ echo "declare const foo: Bar" | nix run ".#tshm" -- -a
foo :: Bar
```

## History

I used to run various different operating systems and window managers. Those configs are preserved in this repo's commit history.

- Arch/XMonad: `nixos~1`
- nix-darwin: `rm-darwin~1`
