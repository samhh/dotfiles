{ ... }:

{
  services.netdata = {
    enable = true;
    python.enable = false;
  };

  networking.firewall.allowedTCPPorts = [
    19999
  ];
}
