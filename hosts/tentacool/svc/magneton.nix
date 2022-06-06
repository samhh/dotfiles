{ ... }:

{
  services.jackett.enable = true;

  networking.firewall.allowedTCPPorts = [
    9117
  ];
}
