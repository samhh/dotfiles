{ ... }:

{
  imports = [
    ./backup.nix
    ./core.nix
    ./hardware.nix
    ./http.nix
    ./network.nix
    ./ssh.nix
    ./virt.nix

    ./svc
  ];
}
