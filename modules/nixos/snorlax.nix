{ nasPath, ... }:

{
  fileSystems.${nasPath} = {
    device = "snorlax:/volume1/media/";
    fsType = "nfs";
    options = [ "nfsvers=4" "noauto" "x-systemd.automount" ];
  };
}
