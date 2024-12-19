{ ... }:

let
  lanPort = 8283;
in
{
  networking.firewall = {
    allowedTCPPorts = [
      lanPort
    ];
  };

  virtualisation.oci-containers.containers = {
    matterbridge = {
      image = "docker.io/luligu/matterbridge:1.6.7";
      volumes = [
        "matterbridge-upper:/root/Matterbridge"
        "matterbridge-dot:/root/.matterbridge"
      ];
      extraOptions = [ "--network=host" ];
    };
  };
}
