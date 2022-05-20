{ config, lib, pkgs, termBin, uname, nasPath, ... }:

let
  home = config.users.users.${uname}.home;
  output = "DP-3";
  barName = "top";
  wallpaperScript = "${home}/dotfiles/hosts/alakazam/scripts/set-rand-wallpaper.sh ${nasPath}/bgs";
in {
  fonts.fonts = with pkgs; [
    hasklig
    noto-fonts-emoji
  ];

  programs.sway.enable = true;
  xdg.portal.wlr.enable = true;

  home-manager.users.${uname} = {
    # Autostart, but only in tty1. Can't use ordinary
    # `environment.loginShellInit` as fish isn't POSIX-compliant.
    programs.fish.loginShellInit = ''
      if test -z $DISPLAY; and test (tty) = /dev/tty1; exec sway; end
    '';

    wayland.windowManager.sway =
      let mod = "Mod4";
      in with lib; {
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
          # Ideally the likes of dmenu would be packaged up in there as well
          # and not exposed in $PATH.
          keybindings = mkOptionDefault {
            "XF86AudioMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioLowerVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -1%";
            "XF86AudioRaiseVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +1%";
            "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
            "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
            "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
            "${mod}+Return" = "exec ${termBin}";
            "${mod}+w" = "exec ${wallpaperScript}";
            "${mod}+t" = "exec ${home}/dotfiles/hosts/alakazam/scripts/web-search.sh";
            "${mod}+g" = "exec ${home}/dotfiles/hosts/alakazam/scripts/apps.sh";
            "${mod}+Shift+g" = "exec ${pkgs.dmenu}/bin/dmenu_run -p gui-all";
            "${mod}+d" = "exec ${home}/dotfiles/hosts/alakazam/scripts/flatmarks.sh";
            "${mod}+Shift+d" = "exec ${home}/dotfiles/hosts/alakazam/scripts/flatmarks-work.sh";
            "${mod}+x" = "exec ${home}/dotfiles/hosts/alakazam/scripts/passmenu.sh";
            "${mod}+n" = "exec ${home}/dotfiles/hosts/alakazam/scripts/pass-prefixed-line.sh \"username: \" username";
            "${mod}+m" = "exec ${home}/dotfiles/hosts/alakazam/scripts/pass-prefixed-line.sh \"email: \" email";
            "${mod}+z" = "exec ${home}/dotfiles/hosts/alakazam/scripts/definition-lookup.sh";
            "${mod}+o" = "exec ${pkgs.mako}/bin/makoctl dismiss";
            "${mod}+Shift+o" = "exec ${pkgs.mako}/bin/makoctl dismiss -a";
            "${mod}+p" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save area";
            "${mod}+l" = "exec ${pkgs.swaylock}/bin/swaylock -c 000000";
            "${mod}+Shift+l" = "exec systemctl suspend";
          };
          assigns = {
            "8" = [{ class = "^Slack$"; }];
            "9" = [{ class = "^discord$"; }];
          };
        };
      };

    programs.i3status-rust = {
      enable = true;
      bars.${barName} = {
        theme = "bad-wolf";
        blocks = let max = 75; in [
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
        command = "${pkgs.swaylock}/bin/swaylock -f -c 000000";
      }];
      timeouts = [{
        timeout = 1800;
        command = "${pkgs.swaylock}/bin/swaylock -f -c 000000";
      }];
    };

    services.gammastep = {
      enable = true;
      # Roughly London.
      latitude = 51.5941;
      longitude = 0.1298;
    };

    systemd.user.services = {
      mako = {
        Install.WantedBy = [ "graphical-session.target" ];
        Service.ExecStart = "${pkgs.mako}/bin/mako";
      };

      wallpaper = {
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          Type = "oneshot";
          ExecStart = wallpaperScript;
        };
      };
    };

    home.packages = with pkgs; [
      # For some scripts.
      bash
      dmenu
      sway-contrib.grimshot
      swaybg
      # For scripts interacting with `swaymsg`.
      jq
    ];
  };
}
