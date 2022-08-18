{ ... }:

{
  imports = [
    ../../modules/core.nix
    ../../modules/locale.nix
    ../../modules/secrets.nix
    ../../modules/security.nix
    ../../modules/snorlax.nix
    ../../modules/user.nix

    ./containers.nix
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
