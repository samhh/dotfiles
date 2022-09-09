{ config, lib, pkgs, ... }:

let
  scripts = "${config.users.users.${config.username}.home}/dotfiles/hosts/alakazam/scripts";
  output = "DP-3";
  barName = "top";

  locker = pkgs.writeShellScriptBin "locker" ''
    img="/tmp/lock.png"

    ${pkgs.sway-contrib.grimshot}/bin/grimshot save screen - | ${pkgs.corrupter}/bin/corrupter - > "$img"
    ${pkgs.swaylock}/bin/swaylock -i "$img" "$@"
  '';
in
{
  fonts.fonts = with pkgs; [
    hasklig
    noto-fonts-emoji
  ];

  programs.sway.enable = true;
  xdg.portal.wlr.enable = true;

  home-manager.users.${config.username} = {
    # Autostart, but only in tty1. Can't use ordinary
    # `environment.loginShellInit` as fish isn't POSIX-compliant.
    programs.fish.loginShellInit = ''
      if test -z $DISPLAY; and test (tty) = /dev/tty1; exec sway; end
    '';

    wayland.windowManager.sway =
      let mod = "Mod4";
      in
      with lib; {
        enable = true;
        # The NixOS wiki says this makes GTK "work properly". /shrug
        wrapperFeatures.gtk = true;
        config = {
          output.${output} = {
            mode = "2560x1440@240Hz";
            adaptive_sync = "on";
          };
          bars = [{
            statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-${barName}.toml";
            position = "top";
          }];
          gaps.inner = 10;
          modifier = mod;
          # Scripts aren't imported into Nix as they have relatively-pathed
          # dependencies upon other scripts Nix that doesn't know about.
          # Ideally the likes of tofi would be packaged up in there as well
          # and not exposed in $PATH.
          keybindings = mkOptionDefault {
            "XF86AudioMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioLowerVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -1%";
            "XF86AudioRaiseVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +1%";
            "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
            "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
            "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
            "${mod}+Return" = "exec ${config.apps.terminal.bin}";
            "${mod}+w" = "exec systemctl --user restart wallpaper";
            "${mod}+t" = "exec ${scripts}/web-search.sh";
            "${mod}+g" = "exec ${scripts}/apps.sh";
            "${mod}+Shift+g" = "exec ${pkgs.tofi}/bin/tofi-run --prompt gui-all | xargs swaymsg exec --";
            "${mod}+d" = "exec ${scripts}/flatmarks.sh";
            "${mod}+Shift+d" = "exec ${scripts}/flatmarks-work.sh";
            "${mod}+x" = "exec ${scripts}/passmenu.sh";
            "${mod}+n" = "exec ${scripts}/pass-prefixed-line.sh \"username: \" username";
            "${mod}+m" = "exec ${scripts}/pass-prefixed-line.sh \"email: \" email";
            "${mod}+z" = "exec ${scripts}/definition-lookup.sh";
            "${mod}+o" = "exec ${pkgs.mako}/bin/makoctl dismiss";
            "${mod}+Shift+o" = "exec ${pkgs.mako}/bin/makoctl dismiss -a";
            "${mod}+p" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save area";
            "${mod}+l" = "exec ${locker}/bin/locker";
            "${mod}+Shift+l" = "exec systemctl suspend";
          };
          assigns = {
            "8" = [{ class = "^Slack$"; }];
          };
          floating.criteria = [{ app_id = "^pinentry-gtk$"; }];
        };
      };

    programs.i3status-rust = {
      enable = true;
      bars.${barName} = {
        theme = "bad-wolf";
        blocks = let max = 75; in
          [
            {
              block = "focused_window";
              max_width = max;
            }
            {
              block = "cpu";
              format = "{utilization}";
            }
            {
              block = "temperature";
              driver = "sensors";
              chip = "k10temp-pci-00c3";
              format = "{max}";
              collapsed = false;
              interval = 1;
              good = 1;
              idle = 50;
              info = 70;
              warning = 85;
            }
            {
              block = "memory";
              display_type = "memory";
              clickable = false;
              format_mem = "{mem_used}";
            }
            {
              block = "net";
            }
            {
              block = "music";
              player = "mpd";
              max_width = max;
              dynamic_width = true;
              format = "{combo} ";
            }
            {
              block = "sound";
            }
            {
              block = "time";
            }
          ];
      };
    };

    services.swayidle = {
      enable = true;
      events = [{
        event = "before-sleep";
        command = "exec ${locker}/bin/locker -f";
      }];
    };

    services.gammastep = {
      enable = true;
      # Roughly London.
      latitude = 51.5941;
      longitude = 0.1298;
    };

    systemd.user.services =
      let wmTarget = "sway-session.target";
      in
      {
        mako = {
          Install.WantedBy = [ wmTarget ];
          Service.ExecStart = "${pkgs.mako}/bin/mako";
        };

        wallpaper = {
          Install.WantedBy = [ wmTarget ];
          Service = {
            ExecStart = "${scripts}/set-rand-wallpaper.sh ${config.nas.path}/bgs";
            Environment =
              let deps = with pkgs; [ coreutils findutils procps swaybg ];
              in [ "PATH=${lib.makeBinPath deps}" ];
            Restart = "always";
            RuntimeMaxSec = "3h";
          };
        };
      };

    xdg.configFile."tofi/config".source = ./cfg/tofi.cfg;

    gtk = {
      enable = true;
      theme = {
        package = pkgs.mojave-gtk-theme;
        name = "Mojave-Light";
      };
    };

    home.packages = with pkgs; [
      sway-contrib.grimshot

      # For various scripts.
      bash
      tofi

      # For scripts interacting with `swaymsg`.
      gron
      jq

      # For backup script.
      tree

      # For export script.
      pandoc
      zip
    ];
  };
}
