{ ... }:

{
  imports = [
    ./core.nix
    ./hardware.nix
    ./network.nix
    ./ssh.nix
    ./virt.nix

    ./svc
  ];
}
