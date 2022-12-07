{ ... }:

let
  lanPort = 8123;
  mDNSPort = 5353;
in
{
  networking.firewall = {
    allowedTCPPorts = [ lanPort ];
    allowedUDPPorts = [ mDNSPort ];
  };

  virtualisation.oci-containers.containers.hass = {
    image = "ghcr.io/home-assistant/home-assistant:2022.12.0";
    volumes = [
      "hass:/config"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment.TZ = "Europe/London";
  };

  services.nginx.virtualHosts."starmie.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString lanPort}";
      proxyWebsockets = true;
    };
  };

  services.ddclient.domains = [ "starmie" ];
}
