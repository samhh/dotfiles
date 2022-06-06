{ ... }:

{
  services.sonarr.enable = true;

  networking.firewall.allowedTCPPorts = [
    8989
  ];
}
