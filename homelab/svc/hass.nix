{ ... }:

let
  lanPort = 8123;
  mDNSPort = 5353;
  matterPort = 5540;
  homekitBridgePort = 21064;
in
{
  networking.firewall = {
    allowedTCPPorts = [
      lanPort
      matterPort
      homekitBridgePort
    ];
    allowedUDPPorts = [
      mDNSPort
      matterPort
    ];
  };

  virtualisation.oci-containers.containers =
    let
      hostNetworking = "--network=host";
    in
    {
      hass = {
        image = "ghcr.io/home-assistant/home-assistant:2024.12.4";
        volumes = [ "hass:/config" ];
        extraOptions = [ hostNetworking ];
        environment.TZ = "Europe/London";
      };

      matter = {
        image = "ghcr.io/home-assistant-libs/python-matter-server:6.6.1";
        volumes = [
          "hass-matter:/data"
          "/run/dbus:/run/dbus:ro"
        ];
        extraOptions = [ hostNetworking ];
      };
    };
}
