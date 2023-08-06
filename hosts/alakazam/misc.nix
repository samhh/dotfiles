{ config, lib, pkgs, ... }:

let
  home = config.users.users.${config.username}.home;
  qrcpPort = 8090;
  vaporeon = pkgs.writeShellScriptBin "vaporeon" ''
    exec ${pkgs.blocky}/bin/blocky --apiHost tentacool "$@"
  '';

in
{
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

  services.vdirsyncer = {
    enable = true;
    jobs.krabby = {
      # So that we can read the decryped secret, which is owned by this user.
      user = config.username;
      group = "users";
      forceDiscover = true;
      config = {
        pairs.contacts = {
          a = "contacts_local";
          b = "contacts_remote";
          collections = [ "from a" "from b" ];
        };
        storages = {
          contacts_local = {
            type = "filesystem";
            path = "~/contacts/";
            fileext = ".vcf";
          };
          contacts_remote = {
            type = "carddav";
            url = "https://krabby.samhh.com";
            username = "sam";
            # Without the quotes the config output will be corrupt.
            "password.fetch" = [ "command" "cat" config.age.secrets.krabby.path ];
          };
        };
      };
    };
  };

  # Hack to allow the vdirsyncer unit to write to $HOME.
  systemd.services."vdirsyncer@krabby".serviceConfig.ProtectHome = lib.mkForce false;

  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ config.username ];
  };

  home-manager.users.${config.username} = {
    xdg.configFile."khard/khard.conf".source = ./cfg/khard.conf;

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

      # Haskell
      "hie.yaml"

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
