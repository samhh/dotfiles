{ ... }:

let
  webPort = 10443;
  cameraPorts = { from = 10450; to = 10460; };
in
{
  networking.firewall = {
    allowedTCPPorts = [ webPort ];
    allowedTCPPortRanges = [ cameraPorts ];
  };

  virtualisation.oci-containers.containers.scrypted = {
    image = "koush/scrypted:18-bullseye-thin-v0.6.26";
    volumes = [
      "scrypted:/server/volume"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment.TZ = "Europe/London";
  };
}
