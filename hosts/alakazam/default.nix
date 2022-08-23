{ ... }:

{
  imports = [
    ../../modules/nixos/core.nix
    ../../modules/nixos/locale.nix
    ../../modules/nixos/secrets.nix
    ../../modules/nixos/security.nix
    ../../modules/nixos/snorlax.nix
    ../../modules/nixos/user.nix

    ../../modules/editor.nix

    ./audio.nix
    ./auth.nix
    ./backup.nix
    ./core.nix
    ./de.nix
    ./email.nix
    ./gaming.nix
    ./hardware.nix
    ./misc.nix
    ./network.nix
    ./terminal.nix
    ./user.nix
    ./vcs.nix
    ./web.nix
  ];
}
