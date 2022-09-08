{ ... }:

let webPort = 7878;
in
{
  services.radarr.enable = true;

  networking.firewall.allowedTCPPorts = [
    webPort
  ];
}
