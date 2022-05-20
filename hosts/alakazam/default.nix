{ ... }:

{
  imports = [
    ../../modules/core.nix
    ../../modules/locale.nix
    ../../modules/security.nix
    ../../modules/user.nix

    # Temporary
    ../tentacool/hass.nix

    ./audio.nix
    ./auth.nix
    ./backup.nix
    ./core.nix
    ./de.nix
    ./editor.nix
    ./email.nix
    ./gaming.nix
    ./hardware.nix
    ./misc.nix
    ./network.nix
    ./rss.nix
    ./terminal.nix
    ./user.nix
    ./vcs.nix
    ./web.nix
  ];
}
