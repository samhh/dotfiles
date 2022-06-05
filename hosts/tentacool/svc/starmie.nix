{ ... }:

{
  networking.firewall.allowedTCPPorts = [
    # HASS (LAN) non-SSL
    8123
  ];

  virtualisation.oci-containers.containers.hass = {
    image = "ghcr.io/home-assistant/home-assistant:2022.5.3";
    volumes = [
      "hass:/config"
    ];
    extraOptions = [
      "--network=host"
      # Zigbee
      "--device=/dev/ttyUSB0"
    ];
    environment.TZ = "Europe/London";
  };

  services.nginx.virtualHosts."starmie.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:8123";
      proxyWebsockets = true;
    };
  };

  services.ddclient.domains = [ "starmie" ];
}
