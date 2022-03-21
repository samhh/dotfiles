{ config, pkgs, ... }:

{
  home.stateVersion = "22.05";

  home.username = "sam";
  home.homeDirectory = "/home/sam";
  xdg.cacheHome = "/home/sam/.cache/";
  xdg.configHome = "/home/sam/.config/";
  xdg.dataHome = "/home/sam/.local/share/";

  programs.home-manager.enable = true;

  home.packages = [
    # DE & WM
    pkgs.xorg.xkill

    # Wallpaper
    pkgs.pywal
    pkgs.jq

    # Cruft
    pkgs.inotify-tools

    # Backup & sync
    pkgs.backblaze-b2
    pkgs.duplicity
    pkgs.offlineimap
    pkgs.vdirsyncer

    # CLI
    pkgs.aerc
    pkgs.bandwhich
    pkgs.beets
    pkgs.delta
    pkgs.direnv
    pkgs.fd
    pkgs.feh
    pkgs.fish
    pkgs.fzf
    pkgs.gdu
    pkgs.gitFull # Non-full doesn't support send-email
    pkgs.gnupg
    pkgs.gnused
    pkgs.gping
    pkgs.gotop
    pkgs.khard
    pkgs.lftp
    pkgs.libqalculate
    pkgs.mpv
    pkgs.mpvScripts.mpris
    pkgs.neovim
    pkgs.newsboat
    pkgs.pass
    pkgs.pavucontrol
    pkgs.playerctl
    pkgs.progress
    pkgs.qrcp
    pkgs.ripgrep
    pkgs.scrot
    pkgs.senpai
    pkgs.streamlink
    pkgs.tree
    pkgs.urlscan
    pkgs.vimpc
    pkgs.which
    pkgs.zathura

    # GUI
    pkgs.chromium
    pkgs.discord
    pkgs.firefox-devedition-bin
    pkgs.obsidian
    pkgs.piper
    pkgs.simplescreenrecorder

    # Dev - editor tooling
    pkgs.nodePackages.bash-language-server
    pkgs.efm-langserver
    pkgs.haskell-language-server
    pkgs.hlint
    pkgs.shellcheck
    pkgs.stylish-haskell
    pkgs.nodePackages.typescript-language-server
    pkgs.nodePackages.vscode-langservers-extracted

    # Dev - cross-project tooling
    pkgs.nodePackages.yalc

    # Work
    pkgs._1password-gui
    pkgs.heroku
    pkgs.slack
  ];

  services.gpg-agent.enable = true;

  services.lorri.enable = true;

  services.redshift = {
    enable = true;
    latitude = "51.5941";
    longitude = "0.1298";
  };

  services.dunst = {
    enable = true;
    settings = {
      global = {
        geometry = "325x5-24+40";
        font = "Monaco 9";
        line_height = 3;
        separator_height = 3;
        padding = 12;
        horizontal_padding = 15;
        separator_color = "#2e3440";
        word_wrap = true;
        corner_radius = 1;
        idle_threshold = 60;
        show_age_threshold = 60;
        format = "<b>%a</b>\n<b>%s</b> %p\n%b";
        markup = "full";
        dmenu = "/usr/bin/rofi -show notif -dmenu";
        browser = "/usr/bin/qutebrowser";
      };

      urgency_low = {
        background = "#3b4252";
        foreground = "#eceff4";
        timeout = 10;
      };

      urgency_normal = {
        background = "#3b4252";
        foreground = "#eceff4";
        timeout = 20;
      };

      urgency_critical = {
        background = "#bf616a";
        foreground = "#eceff4";
        timeout = 0;
      };
    };
  };
}
