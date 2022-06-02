{ ... }:

{
  imports = [
    ../../modules/core.nix
    ../../modules/locale.nix
    ../../modules/security.nix
    ../../modules/user.nix

    ./hardware.nix
    ./hass.nix
    ./network.nix
    ./ssh.nix
  ];
}
