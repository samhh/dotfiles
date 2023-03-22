{ config, pkgs, ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      timeout = 30;
      systemd-boot = {
        enable = true;
        # My boot partition is only 100MB. This leaves space for only one
        # fallback/named boot entry and one rolling.
        configurationLimit = 1;
      };
    };

    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "sd_mod" ];
      kernelModules = [ "amdgpu" ];
      luks.devices.enc.device =
        "/dev/disk/by-uuid/2b5f2588-4a5b-4645-b812-a7d16196fce8";
    };

    kernelModules = [ "kvm-amd" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e324d67e-bfc0-47a1-83e1-87cc99221c74";
    fsType = "btrfs";
    options = [ "compress=zstd" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/9674-A7F9";
    fsType = "vfat";
  };

  fileSystems."/mnt/games" = {
    device = "/dev/disk/by-uuid/621d4104-82d8-401e-be63-6953e11698fb";
    fsType = "ext4";
  };

  fileSystems."/mnt/games2" = {
    device = "/dev/disk/by-uuid/f074045b-879f-4548-810a-e88a1d140783";
    fsType = "btrfs";
    options = [ "compress=zstd" ];
  };

  hardware = {
    enableRedistributableFirmware = true;
    cpu.amd.updateMicrocode = true;
    video.hidpi.enable = true;
    opengl.enable = true;
    bluetooth.enable = true;
  };

  # systemd-oomd is unhelpful unless everything is plugged into cgroups:
  #   https://github.com/nixos/nixpkgs/issues/113903#issuecomment-857296349
  #
  # earlyoom on the other hand is a helpful emergency brake when memory usage
  # spirals.
  systemd.oomd.enable = false;
  services.earlyoom = {
    enable = true;
    killHook = pkgs.writeShellScript "earlyoom-kill-hook" ''
      ${pkgs.libnotify}/bin/notify-send -u critical \
        "Process \"$EARLYOOM_NAME\" terminated" \
        "System OOM killer triggered."
    '';
  };

  services.ddccontrol.enable = true;

  services.blueman.enable = true;

  services.ratbagd.enable = true;

  home-manager.users.${config.username} = {
    services.blueman-applet.enable = true;

    home.packages = with pkgs; [
      lm_sensors
      piper
    ];
  };
}
