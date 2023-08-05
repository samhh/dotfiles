{ ... }:

{
  imports = [
    ./backup.nix
    ./core.nix
    ./ddns.nix
    ./hardware.nix
    ./http.nix
    ./network.nix
    ./secrets.nix
    ./ssh.nix
    ./virt.nix

    ./svc
  ];
}
