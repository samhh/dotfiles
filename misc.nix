{ pkgs, uname, ... }:

let
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
  virtualisation.podman.enable = true;

  home-manager.users.${uname} = {
    programs.senpai = {
      enable = true;
      config = {
        addr = "chat.sr.ht";
	nick = "samhh";
	password-cmd = "${pkgs.pass}/bin/pass show _irc/chat.sr.ht/token";
      };
    };

    xdg.configFile."khard/khard.conf".source = ./cfg/khard.conf;

    home.packages = with pkgs; [
      # CLI
      bandwhich
      distrobox
      fd
      gdu
      gnupg
      gping
      gotop
      sway-contrib.grimshot
      imv
      khard
      lftp
      libnotify
      libqalculate
      mpv
      pass
      qrcp
      ripgrep
      gnused
      streamlink
      tree
      unrar
      unzip
      vimpc
      wf-recorder
      wl-clipboard
      # zathura

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
