{ ... }:

let webPort = 19999;
in
{
  services.netdata = {
    enable = true;
    python.enable = false;
  };

  networking.firewall.allowedTCPPorts = [
    webPort
  ];
}
