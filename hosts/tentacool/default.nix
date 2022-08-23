{ ... }:

{
  imports = [
    ../../modules/nixos

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
