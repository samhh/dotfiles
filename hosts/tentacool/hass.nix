{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    certbot
  ];

  networking.firewall.allowedTCPPorts = [
    # Let's Encrypt challenge
    80
    # HASS
    8123
  ];

  virtualisation.oci-containers = {
    backend = "podman";
    containers.hass = {
      image = "ghcr.io/home-assistant/home-assistant:2022.5.3";
      volumes = [
        "hass:/config"
        # Mounting at a fairly high path due to the presence of relative symlinks.
        "/etc/letsencrypt:/ssl"
      ];
      extraOptions = [
        "--network=host"
        # Zigbee
        "--device=/dev/ttyUSB0"
      ];
      environment.TZ = "Europe/London";
    };
  };
}
