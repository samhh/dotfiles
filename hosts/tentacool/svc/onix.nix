{ config, ... }:

let
  dnsPort = 53;
  webPort = 8053;
in
{
  networking.firewall.allowedTCPPorts = [
    dnsPort
    webPort
  ];
  networking.firewall.allowedUDPPorts = [
    dnsPort
  ];

  virtualisation.oci-containers.containers.pihole = {
    image = "pihole/pihole:2022.09.2";
    volumes = [
      "pihole:/etc/pihole"
      "dnsmasq:/etc/dnsmasq.d"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment = {
      WEB_PORT = toString webPort;
      TZ = "Europe/London";
    };
    environmentFiles = [
      config.age.secrets.pihole-env.path
    ];
  };
}
