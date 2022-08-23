{ ... }:

{
  imports = [
    ../../modules/nixos/core.nix
    ../../modules/nixos/locale.nix
    ../../modules/nixos/secrets.nix
    ../../modules/nixos/security.nix
    ../../modules/nixos/snorlax.nix
    ../../modules/nixos/user.nix

    ./ddns.nix
    ./hardware.nix
    ./http.nix
    ./network.nix
    ./secrets.nix
    ./ssh.nix

    ./svc/blastoise.nix
    ./svc/chansey.nix
    ./svc/exeggutor.nix
    ./svc/krabby.nix
    ./svc/magneton.nix
    ./svc/metapod.nix
    ./svc/onix.nix
    ./svc/poliwhirl.nix
    ./svc/starmie.nix
    ./svc/vulpix.nix
  ];
}
