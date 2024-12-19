{ ... }:

let
  lanPort = 8283;
  mDNSPort = 5353;
  matterPort = 5540;
in
{
  networking.firewall = {
    allowedTCPPorts = [
      lanPort
      matterPort
    ];
    allowedUDPPorts = [
      matterPort
      mDNSPort
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
