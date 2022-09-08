{ ... }:

let webPort = 9117;
in
{
  services.jackett.enable = true;

  networking.firewall.allowedTCPPorts = [
    webPort
  ];
}
