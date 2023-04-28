{ config, pkgs, ... }:

let
  uid = config.users.users.${config.username}.uid;
in
{
  security.doas.extraRules = [{
    users = [ config.username ];
    keepEnv = true;
  }];

  home-manager.users.${config.username} = {
    programs.foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          font = "Hasklig:size=11";
          pad = "10x10";
        };
        key-bindings.spawn-terminal = "Mod4+Shift+Return";
        url.launch = "${config.apps.webBrowser.bin} \${url}";
        colors = {
          alpha = .92;

          # Non-base 16 from:
          #   https://github.com/rebelot/kanagawa.nvim/blob/f3c1bb8b6f67b26a2d39babf86d0807c4004c28e/extras/kanagawa.conf
          #
          # As per:
          #   https://github.com/rebelot/kanagawa.nvim/issues/58#issuecomment-1184386932
          #
          # Except the background, which is translucent near-black. I prefer it
          # without the blueish hue.
          foreground = config.colorScheme.colors.base05;
          background = "121212";
          selection-foreground = config.colorScheme.colors.base04;
          selection-background = "2d4f67";
          regular0 = "090618";
          regular1 = config.colorScheme.colors.base08;
          regular2 = "76946a";
          regular3 = "c0a36e";
          regular4 = config.colorScheme.colors.base0D;
          regular5 = config.colorScheme.colors.base0E;
          regular6 = "6a9589";
          regular7 = config.colorScheme.colors.base04;
          bright0 = config.colorScheme.colors.base03;
          bright1 = "e82424";
          bright2 = config.colorScheme.colors.base0B;
          bright3 = "e6c384";
          bright4 = config.colorScheme.colors.base0C;
          bright5 = config.colorScheme.colors.base06;
          bright6 = "7aa89f";
          bright7 = config.colorScheme.colors.base05;
          "16" = config.colorScheme.colors.base09;
          "17" = "ff5d62";
        };
      };
    };

    programs.fish = {
      shellInit = ''
        set -x SSH_AUTH_SOCK /run/user/${toString uid}/ssh-agent
      '';

      shellAbbrs = {
        "mann" = "tldr";
        "find" = "fd";
        "tree" = "tre";
        "sed" = "sd";
        "df" = "duf";
        "du" = "gdu";
        "ping" = "gping";
        "mpc" = "vimpc";
        "top" = "gotop";
        "cat" = "bat";

        "sys" = "systemctl";
        "sysu" = "systemctl --user";
        "up" = "nixos-rebuild --flake .# build";
        "upp" = "doas nixos-rebuild --flake .# switch";
      };
    };

    home.packages = with pkgs; [
      upterm
    ];
  };
}
