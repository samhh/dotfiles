{ ... }:

let webPort = 8989;
in
{
  services.sonarr.enable = true;

  networking.firewall.allowedTCPPorts = [
    webPort
  ];
}
