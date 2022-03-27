{ config, pkgs, ... }:

let
  uname = "sam";
  home = /. + config.users.users.${uname}.home;

  qbpm = pkgs.callPackage /home/sam/nix/pkg/qbpm {};
  banginServerNode = pkgs.callPackage /home/sam/nix/pkg/bangin-server-node.nix {};

  banginServerNodePort = 1234;
in {
  home-manager.users.${uname} = {
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "text/html" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
      };
    };

    programs.qutebrowser = {
      enable = true;
      searchEngines.DEFAULT = "http://localhost:${toString banginServerNodePort}/?q={}";
      settings = {
        confirm_quit = [ "downloads" ];
        content = {
          blocking.enabled = false;
          cookies.accept = "no-3rdparty";
          javascript.enabled = true;
          notifications.enabled = false;
          unknown_url_scheme_policy = "allow-all";
        };
        editor.command = [ "foot" "nvim" "{}" ];
        # Colemak home row keys.
        hints.chars = "arstneio";
        scrolling = {
          bar = "never";
          smooth = true;
        };
        tabs = {
          last_close = "close";
          position = "bottom";
          show = "multiple";
        };
        url.default_page = "about:blank";
      };
      keyBindings.normal = {
        gT = "tab-prev";
        gt = "tab-next";
        gH = "back";
        gh = "forward";
        X  = "spawn --userscript qute-pass --username-target secret --username-pattern \"username: (.+)\"";
        V  = "spawn streamlink {url}";

        b  = "nop";
        B  = "nop";
        m  = "nop";
        M  = "nop";
      };
    };

    systemd.user.services.banginServerNode = {
      Install.WantedBy = [ "default.target" ];
      Service.ExecStart =
        "${banginServerNode}/bin/bangin-server-node ${toString banginServerNodePort}";
    };

    home.packages = with pkgs; [
      ungoogled-chromium
      firefox-wayland
      qbpm
    ];

    xdg.dataFile."bangin/lists/arch.bangs".source = home + /dev/arch.bangs/arch.bangs;
    xdg.dataFile."bangin/lists/dev.bangs".source = home + /dev/dev.bangs/dev.bangs;
    xdg.dataFile."bangin/lists/english.bangs".source = home + /dev/english.bangs/english.bangs;
    xdg.dataFile."bangin/lists/italiano.bangs".source = home + /dev/italiano.bangs/italiano.bangs;
    xdg.dataFile."bangin/lists/pcgaming.bangs".source = home + /dev/pcgaming.bangs/pcgaming.bangs;
    xdg.dataFile."bangin/lists/prelude.bangs".source = home + /dev/prelude.bangs/prelude.bangs;
    xdg.dataFile."bangin/lists/trackers.bangs".source = home + /dev/trackers.bangs/trackers.bangs;
    xdg.dataFile."bangin/lists/uk.bangs".source = home + /dev/uk.bangs/uk.bangs;
  };
}
