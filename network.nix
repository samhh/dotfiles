{ ... }:

{
  networking = {
    hostName = "alakazam";

    interfaces.enp42s0.useDHCP = true;
  };

  services.printing.enable = true;

  fileSystems."/mnt/nas" = {
    device = "snorlax:/volume1/media/";
    fsType = "nfs";
    options = [ "nfsvers=4" "noauto" "x-systemd.automount" ];
  };
}
