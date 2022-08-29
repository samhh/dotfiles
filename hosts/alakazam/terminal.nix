{ config, pkgs, uname, webBrowserBin, ... }:

let
  uid = config.users.users.${uname}.uid;
in
{
  security.doas.extraRules = [{
    users = [ uname ];
    keepEnv = true;
  }];

  home-manager.users.${uname} = {
    programs.foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          font = "Hasklig:size=11";
          pad = "10x10";
        };
        key-bindings.spawn-terminal = "Mod4+Shift+Return";
        url.launch = "${webBrowserBin} \${url}";
        colors = {
          alpha = .92;

          # From: https://github.com/rebelot/kanagawa.nvim/blob/master/extras/foot_kanagawa.ini
          foreground = "dcd7ba";
          # background = "1f1f28";
          selection-foreground = "c8c093";
          selection-background = "2d4f67";
          regular0 = "090618";
          regular1 = "c34043";
          regular2 = "76946a";
          regular3 = "c0a36e";
          regular4 = "7e9cd8";
          regular5 = "957fb8";
          regular6 = "6a9589";
          regular7 = "c8c093";
          bright0 = "727169";
          bright1 = "e82424";
          bright2 = "98bb6c";
          bright3 = "e6c384";
          bright4 = "7fb4ca";
          bright5 = "938aa9";
          bright6 = "7aa89f";
          bright7 = "dcd7ba";
          "16" = "ffa066";
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

        "sys" = "systemctl";
        "sysu" = "systemctl --user";
        "up" = "nixos-rebuild build";
        "upp" = "doas nixos-rebuild switch";
      };
    };
  };
}
