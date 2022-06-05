{ ... }:

{
  imports = [
    ../../modules/core.nix
    ../../modules/locale.nix
    ../../modules/security.nix
    ../../modules/snorlax.nix
    ../../modules/user.nix

    ./containers.nix
    ./ddns.nix
    ./hardware.nix
    ./http.nix
    ./network.nix
    ./ssh.nix

    ./svc/blastoise.nix
    ./svc/onix.nix
    ./svc/starmie.nix
  ];
}
