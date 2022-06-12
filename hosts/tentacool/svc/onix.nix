{ config, ... }:

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
      # Ideally we wouldn't `readFile`:
      #   https://github.com/ryantm/agenix/tree/7e5e58b98c3dcbf497543ff6f22591552ebfe65b#builtinsreadfile-anti-pattern
      WEBPASSWORD = builtins.readFile config.age.secrets.pihole-pass.path;
      TZ = "Europe/London";
    };
  };
}
