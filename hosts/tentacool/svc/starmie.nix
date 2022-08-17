{ ... }:

{
  networking.firewall.allowedTCPPorts = [
    # HASS (LAN) non-SSL
    8123
    # Zigbee2MQTT frontend
    8080
  ];

  virtualisation.oci-containers.containers.hass = {
    image = "ghcr.io/home-assistant/home-assistant:2022.8.5";
    volumes = [
      "hass:/config"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment.TZ = "Europe/London";
  };

  services.mosquitto = {
    enable = true;
    # Implies local-only mode, which works out of the box where for some reason
    # an `allow_anonymous` listener doesn't.
    listeners = [ ];
  };

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      homeassistant = true;
      permit_join = true;
      serial.port = "/dev/ttyUSB0";
      frontend = true;
    };
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
