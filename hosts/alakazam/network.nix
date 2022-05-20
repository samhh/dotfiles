{ nasPath, ... }:

{
  networking = {
    hostName = "alakazam";
  };

  services.printing.enable = true;

  fileSystems.${nasPath} = {
    device = "snorlax:/volume1/media/";
    fsType = "nfs";
    options = [ "nfsvers=4" "noauto" "x-systemd.automount" ];
  };
}
