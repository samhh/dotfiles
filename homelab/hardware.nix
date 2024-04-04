{ ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      timeout = 5;
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
      };
    };

    initrd.availableKernelModules = [
      "xhci_pci"
      "ahci"
      "usbhid"
      "sd_mod"
    ];
    kernelModules = [ "kvm-intel" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/b286966c-de98-4957-bd08-1a66f7e15d87";
    fsType = "btrfs";
    options = [ "compress=zstd" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1ACB-435B";
    fsType = "vfat";
  };

  hardware = {
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };
}
