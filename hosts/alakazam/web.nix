{ config, pkgs, ... }:

let
  editorBin = "${config.home-manager.users.${config.username}.programs.neovim.finalPackage}/bin/nvim";
  scripts = "${config.users.users.${config.username}.home}/dotfiles/hosts/alakazam/scripts";

  banginServerNodePort = 1234;

  desktopName = "qutebrowserConditional";
in
{
  home-manager.users.${config.username} = {
    xdg.desktopEntries.${desktopName} = {
      name = "Qutebrowser (profile-conditional)";
      exec = "${scripts}/browser-launch.sh";
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let dt = "${desktopName}.desktop";
        in
        {
          "text/html" = dt;
          "x-scheme-handler/http" = dt;
          "x-scheme-handler/https" = dt;
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
        editor.command = [ config.apps.terminal.bin editorBin "{}" ];
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
        x =
          "spawn --userscript qute-pass --username-target secret --username-pattern \"username: (.+)\" --dmenu-invocation ${config.apps.launcher.bin}";
        v = "spawn ${pkgs.mpv}/bin/mpv --title=Picture-in-Picture {url}";

        b = "nop";
        B = "nop";
        m = "nop";
        M = "nop";
      };
    };

    programs.bangin = {
      enable = true;
      lists =
        let f = x: "https://git.sr.ht/~samhh/${x}.bangs/blob/master/${x}.bangs";
        in
        map f [
          "arch"
          "dev"
          "english"
          "italiano"
          "nix"
          "pcgaming"
          "prelude"
          "uk"
        ];
    };

    services.bangin-server-node = {
      enable = true;
      port = banginServerNodePort;
    };

    home.packages = with pkgs; [
      bangup
      ungoogled-chromium
      firefox-wayland
      qbpm
    ];

    xdg.dataFile."qutebrowser/userscripts/qute-pass".source =
      "${pkgs.qutebrowser}/share/qutebrowser/userscripts/qute-pass";
  };
}
