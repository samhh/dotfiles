{ config, pkgs, ... }:

let
  scripts = "${config.users.users.${config.username}.home}/dotfiles/hosts/alakazam/scripts";
  desktopName = "workspaceConditionalBrowser";
in
{
  services.bangin-server-node = {
    enable = true;
    port = 1234;
    fallback = "https://kagi.com/search?q={{{s}}}";
  };

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

    programs.librewolf = {
      enable = true;
      settings = {
        "privacy.clearOnShutdown.cookies" = false;
        "privacy.clearOnShutdown.cache" = false;
        "media.autoplay.blocking_policy" = 2;
        "webgl.disabled" = false;
        # Breaks dark mode. However...: https://amiunique.org/fp
        "privacy.resistFingerprinting" = false;
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

    home.packages = with pkgs; [
      bangup
      ungoogled-chromium
      firefox-wayland
    ];
  };
}
