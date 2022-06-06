{ ... }:

{
  services.radarr.enable = true;

  networking.firewall.allowedTCPPorts = [
    7878
  ];
}
