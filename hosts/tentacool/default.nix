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

    ./svc
  ];
}
