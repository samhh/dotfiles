# dotfiles

Configuring the universe.

## Usage

Build the Home Manager config for macOS.

```console
$ home-manager switch --flake path/to/repo
```

Install tools with Homebrew as needed rather than maintaining a package inventory. Homebrew itself is installed manually, like Nix. The current config expects these tools:

```console
$ brew install git jj helix starship zoxide rustup
```

## History

Over the years I've run various different operating systems, window managers, and editors. Those configs are preserved in this repo's commit history:

- NixOS/Home Assistant: `rm-homelab~1`
- Helix: `zed~1`
- NixOS/Sway/Neovim: `macos-redux~1`
- Arch/XMonad: `nixos~1`
- nix-darwin: `rm-darwin~1`
