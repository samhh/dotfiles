{ config, pkgs, ... }:

let
  editorBin = "${config.home-manager.users.${config.username}.programs.neovim.finalPackage}/bin/nvim";
  mpvBin = "${config.home-manager.users.${config.username}.programs.mpv.finalPackage}/bin/mpv";
  scripts = "${config.users.users.${config.username}.home}/dotfiles/hosts/alakazam/scripts";

  desktopName = "workspaceConditionalBrowser";
in
{
  home-manager.users.${config.username} = {
    xdg.desktopEntries.${desktopName} = {
      name = "LibreWolf (profile-conditional)";
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
      searchEngines.DEFAULT = "https://tauros.samhh.com/?q={}";
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
        # Ideally we'd support dynamic light/dark themes, but until then let's
        # avoid some blindingly bright pages at nighttime.
        colors.webpage.preferred_color_scheme = "dark";
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
        v = "spawn ${mpvBin} --title=Picture-in-Picture {url}";

        b = "nop";
        B = "nop";
        m = "nop";
        M = "nop";
      };
    };

    programs.librewolf = {
      enable = true;
      settings = {
        "privacy.clearOnShutdown.cookies" = false;
        "privacy.clearOnShutdown.cache" = false;
        "media.autoplay.blocking_policy" = 2;
      };
    };

    home.packages = with pkgs; [
      ungoogled-chromium
      firefox-wayland
      qbpm
    ];

    xdg.dataFile."qutebrowser/userscripts/qute-pass".source =
      "${pkgs.qutebrowser}/share/qutebrowser/userscripts/qute-pass";
  };
}
