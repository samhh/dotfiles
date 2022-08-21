{ config, pkgs, termBin, launcherBin, streamerBin, uname, ... }:

let
  qbpm = pkgs.callPackage ../../pkg/qbpm.nix {};
  banginServerNode = pkgs.callPackage ../../pkg/bangin-server-node.nix {};
  bangup = pkgs.callPackage ../../pkg/bangup/default.nix {};

  editorBin = "${config.home-manager.users.${uname}.programs.neovim.finalPackage}/bin/nvim";
  scripts = "${config.users.users.${uname}.home}/dotfiles/hosts/alakazam/scripts";

  banginServerNodePort = 1234;

  desktopName = "qutebrowserConditional";
in {
  home-manager.users.${uname} = {
    xdg.desktopEntries.${desktopName} = {
      name = "Qutebrowser (profile-conditional)";
      exec = "${scripts}/browser-launch.sh";
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let dt = "${desktopName}.desktop";
        in {
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
        editor.command = [ termBin editorBin "{}" ];
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
        x  =
          "spawn --userscript qute-pass --username-target secret --username-pattern \"username: (.+)\" --dmenu-invocation ${launcherBin}";
        v  = "spawn ${streamerBin} {url}";

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
      bangup
      ungoogled-chromium
      firefox-wayland
      qbpm
    ];

    xdg.configFile."bangin/bangin.lists".source = ./cfg/bangin.lists;

    xdg.dataFile."qutebrowser/userscripts/qute-pass".source =
      "${pkgs.qutebrowser}/share/qutebrowser/userscripts/qute-pass";
  };
}
