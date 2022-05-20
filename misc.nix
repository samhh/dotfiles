{ config, pkgs, uname, ... }:

let
  home = config.users.users.${uname}.home;

  # Contains this fix for Arch (not in <=1.2.14):
  #   https://github.com/89luca89/distrobox/issues/221
  distrobox = pkgs.distrobox.overrideAttrs (attrs: rec {
    version = "e6342a05c3cc83d612bd05934a2025cceb86af32";
    src = builtins.fetchurl {
      url = "https://github.com/89luca89/distrobox/archive/${version}.tar.gz";
      sha256 = "1ama58gv2cw48pxic9fjhjv4vvxj813i7wigb238jsdbqxryr4qp";
    };
  });
in {
  # Fixes Teensy loader flashing Ergodox EZ:
  #   https://github.com/zsa/docs/issues/14
  services.udev.extraRules = ''
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"
  '';

  home-manager.users.${uname} = {
    xdg.configFile."khard/khard.conf".source = ./cfg/khard.conf;
    xdg.configFile."senpai/senpai.scfg".source = ./cfg/senpai.scfg;

    home.packages = with pkgs; [
      # CLI
      bandwhich
      distrobox
      dogdns
      duf
      fd
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
      mpv
      qrcp
      ripgrep
      qrencode
      sd
      senpai
      streamlink
      tldr
      tre-command
      unrar
      unzip
      vimpc
      wf-recorder
      wl-clipboard
      zathura

      # GUI
      discord
      obsidian

      # Dev
      shellcheck
      nodePackages.yalc

      # Work
      _1password-gui
      slack
    ];

    programs.password-store = {
      enable = true;
      settings.PASSWORD_STORE_DIR = "${home}/passwords/";
    };

    programs.git.ignores = [
      # Yalc
      ".yalc/"
      "yalc.lock"

      # PureScript
      ".psc-ide-port"
      ".psci_modules/"

      # Haskell
      "hie.yaml"

      # npm
      ".npmrc"
    ];
  };
}
