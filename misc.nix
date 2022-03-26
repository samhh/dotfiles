{ pkgs, ... }:

let
  uname = "sam";
in {
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
      zathura

      # GUI
      discord
      obsidian

      # Dev
      nodePackages.yalc

      # Work
      _1password-gui
      slack
      # zoom-us
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
