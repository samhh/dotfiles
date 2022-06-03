{ ... }:

{
  networking.firewall.allowedTCPPorts = [
    # Pi-Hole
    8053
    # DNS
    53
  ];
  networking.firewall.allowedUDPPorts = [
    # DNS
    53
  ];

  virtualisation.oci-containers.containers.pihole = {
    image = "pihole/pihole:2022.05";
    volumes = [
      "pihole:/etc/pihole"
      "dnsmasq:/etc/dnsmasq.d"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment = {
      WEB_PORT = "8053";
      TZ = "Europe/London";
    };
  };
}
