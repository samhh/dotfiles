{ hostName, nasPath, ... }:

{
  networking = {
    inherit hostName;

    interfaces.enp42s0.useDHCP = true;
  };

  services.printing.enable = true;

  fileSystems.${nasPath} = {
    device = "snorlax:/volume1/media/";
    fsType = "nfs";
    options = [ "nfsvers=4" "noauto" "x-systemd.automount" ];
  };
}
