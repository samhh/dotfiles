{ ... }:

{
  imports = [
    ../../modules/core.nix
    ../../modules/locale.nix
    ../../modules/security.nix
    ../../modules/user.nix

    ./containers.nix
    ./hardware.nix
    ./http.nix
    ./network.nix
    ./ssh.nix

    ./svc/onix.nix
    ./svc/starmie.nix
  ];
}
