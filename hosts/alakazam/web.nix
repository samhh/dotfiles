{ config, pkgs, ... }:

let
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

    home.packages = with pkgs; [
      ungoogled-chromium
      firefox-wayland
    ];
  };
}
