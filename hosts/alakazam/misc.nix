{ config, lib, pkgs, ... }:

let
  home = config.users.users.${config.username}.home;
  qrcpPort = 8090;
  vaporeon = pkgs.writeShellScriptBin "vaporeon" ''
    exec ${pkgs.blocky}/bin/blocky --apiHost tentacool "$@"
  '';

in
{
  age.secrets.irc-token = {
    file = ../../secrets/irc-token.age;
    # For senpai.
    owner = config.username;
  };

  networking.firewall.allowedTCPPorts = [
    qrcpPort
  ];

  # Fixes Teensy loader flashing Ergodox EZ:
  #   https://github.com/zsa/docs/issues/14
  services.udev.extraRules = ''
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"
  '';

  virtualisation.podman.enable = true;

  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ config.username ];
  };

  home-manager.users.${config.username} = {
    # The Home Manager config module is outdated. See:
    #   https://github.com/nix-community/home-manager/issues/2534
    xdg.configFile."senpai/senpai.scfg".text = ''
      address chat.sr.ht
      nickname samhh
      password-cmd cat ${config.age.secrets.irc-token.path}
    '';

    home.packages = with pkgs; [
      # CLI
      bandwhich
      bat
      distrobox
      dogdns
      duf
      fd
      ffmpeg_6-full # For `ffplay`.
      gdu
      gnupg
      gping
      gotop
      sway-contrib.grimshot
      hyperfine
      imv
      khard
      lftp
      libnotify
      libqalculate
      nix-alien
      nvd
      qrcp
      ripgrep
      qrencode
      sd
      senpai
      tldr
      tre-command
      unzip
      vaporeon
      vimpc
      wf-recorder
      wl-clipboard
      zathura

      # GUI
      obsidian
      xdg_utils # Required by Obsidian for opening links on click.

      # Dev
      shellcheck
      nodePackages.yalc

      # Work
      slack
    ];

    # Fix qrcp port so we can allow it through firewall.
    programs.fish.shellInit = ''
      set -x QRCP_PORT ${toString qrcpPort}
    '';

    programs.git.ignores = [
      # Yalc
      ".yalc/"
      "yalc.lock"

      # PureScript
      ".psc-ide-port"
      ".psci_modules/"

      # npm
      ".npmrc"

      # Obsidian
      ".obsidian"
      ".trash"
    ];

    programs.mpv = {
      enable = true;
      package = pkgs.mpv.override {
        scripts = with pkgs.mpvScripts; [ mpris ];
      };
      config.volume = 65;
      bindings.MBTN_LEFT = "cycle pause";
    };
  };
}
