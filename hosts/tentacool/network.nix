{ ... }:

{
  networking = {
    hostName = "tentacool";
    dhcpcd.enable = false;
  };

  systemd.network = {
    enable = true;
    networks."10-lan" = {
      matchConfig.Name = "eno1";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      ipv6AcceptRAConfig.Token = "static:::1:2:3:4";
      linkConfig.RequiredForOnline = "routable";
    };
  };
}
