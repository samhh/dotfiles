{ config, ... }:

let
  webPort = 8800;
in
{
  networking.firewall.allowedTCPPorts = [ webPort ];

  virtualisation.oci-containers.containers.seedsync = {
    image = "ipsingh06/seedsync:0.8.6";
    volumes = [
      "${config.nas.path}/seedbox-synced:/downloads"
      "seedsync_cfg:/config"
      # This'll need to be set up externally, both on the host and any remotes.
      "/home/${config.username}/.ssh:/home/seedsync/.ssh"
    ];
    ports = [ (let p = toString webPort; in "${p}:${p}") ];
    environment.TZ = "Europe/London";
  };
}
