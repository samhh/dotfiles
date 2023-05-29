{ config, ... }:

# Mount by IP rather than hostname. This is for Tentacool, on which DNS is
# hosted, which won't have access to its own DNS at boot when it tries to mount
# Snorlax over the network.
let ip = "192.168.0.83";
in
{
  fileSystems.${config.nas.path} = {
    device = "${ip}:/volume1/media/";
    fsType = "nfs";
    options = [ "nfsvers=4" "noauto" "x-systemd.automount" ];
  };
}
