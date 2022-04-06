{ config, pkgs, ... }:

let
  uname = "sam";

  qbpm = pkgs.callPackage ./pkg/qbpm.nix {};
  banginServerNode = pkgs.callPackage ./pkg/bangin-server-node.nix {};

  getBanglist = x: builtins.fetchurl "https://git.sr.ht/~samhh/${x}.bangs/blob/master/${x}.bangs";

  banglists = [ "arch" "dev" "english" "italiano" "nix" "pcgaming" "prelude" "uk" ];

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
        f = "hint all current";
        F = "hint all tab";
        T = "tab-prev";
        t = "tab-next";
        H = "back";
        h = "forward";
        yf = "hint links yank";
        x  = "spawn --userscript qute-pass --username-target secret --username-pattern \"username: (.+)\"";
        v  = "spawn streamlink {url}";

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

    xdg.dataFile =
      let go = x: {
        name = "bangin/lists/${x}.bangs";
        value.source = getBanglist x;
      };
      in
        {
          "qutebrowser/userscripts/qute-pass".source =
            "${pkgs.qutebrowser}/share/qutebrowser/userscripts/qute-pass";
        } // builtins.listToAttrs (map go banglists);
  };
}
