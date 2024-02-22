{ ... }:

let
  lanPort = 8123;
  mDNSPort = 5353;
  homekitBridgePort = 21064;
in
{
  networking.firewall = {
    allowedTCPPorts = [ lanPort homekitBridgePort ];
    allowedUDPPorts = [ mDNSPort ];
  };

  virtualisation.oci-containers.containers =
    let hostNetworking = "--network=host";
    in
    {
      hass = {
        image = "ghcr.io/home-assistant/home-assistant:2024.2.3";
        volumes = [
          "hass:/config"
        ];
        extraOptions = [ hostNetworking ];
        environment.TZ = "Europe/London";
      };

      matter = {
        image = "ghcr.io/home-assistant-libs/python-matter-server:5.1.4";
        volumes = [
          "hass-matter:/data"
          "/run/dbus:/run/dbus:ro"
        ];
        extraOptions = [ hostNetworking ];
      };
    };
}
