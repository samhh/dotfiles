{ config, lib, nix-colors, pkgs, ... }:

let
  scripts = "${config.users.users.${config.username}.home}/dotfiles/hosts/alakazam/scripts";
  output = "DP-3";
  barName = "top";
  res = { w = 2560; h = 1440; r = 240; };
  barHeight = 24;
  gap = 10;

  # Matches both Firefox and any other windows following this schema.
  pipWindowTitleRegex = "^Picture-in-Picture$";

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

  # Autostart WM only in TTY1.
  environment.loginShellInit = ''
    if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then exec sway; fi
  '';

  colorScheme = nix-colors.colorSchemes.kanagawa;

  home-manager.users.${config.username} = {
    wayland.windowManager.sway =
      let
        mod = "Mod4";
        mprisPlayers = lib.concatStringsSep "," [ "mpv" "firefox" "qutebrowser" "mpd" ];
      in
      with lib; {
        enable = true;
        # The NixOS wiki says this makes GTK "work properly". /shrug
        wrapperFeatures.gtk = true;
        config = {
          output.${output} = {
            mode = with res; "${toString w}x${toString h}@${toString r}Hz";
            adaptive_sync = "on";
          };
          bars = [{
            statusCommand = "${pkgs.waybar}/bin/waybar";
            position = "top";
          }];
          gaps.inner = gap;
          modifier = mod;
          # Scripts aren't imported into Nix as they have relatively-pathed
          # dependencies upon other scripts Nix that doesn't know about.
          # Ideally the likes of tofi would be packaged up in there as well
          # and not exposed in $PATH.
          keybindings = mkOptionDefault {
            "XF86AudioMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioLowerVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -1%";
            "XF86AudioRaiseVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +1%";
            "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl -p \"${mprisPlayers}\" play-pause";
            "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl -p \"${mprisPlayers}\" previous";
            "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl -p \"${mprisPlayers}\" next";
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
          window.commands = [{
            criteria.title = pipWindowTitleRegex;
            command =
              let
                # 16:9
                win = { w = 800; h = 450; };
                # The gap is actually double that configured for some reason,
                # so this is `gap * 2` in spirit.
                spacing = gap * 3;
                pos = { w = res.w - win.w - spacing; h = res.h - win.h - spacing - barHeight; };
              in
              "floating enable; sticky enable; border none; resize set ${toString win.w} ${toString win.h}; move position ${toString pos.w} ${toString pos.h}";
          }];
        };
      };

    programs.waybar = {
      enable = true;
      settings = {
        ${barName} = {
          position = "top";
          height = barHeight;

          modules-left = [ "sway/workspaces" "sway/mode" ];
          modules-center = [ "sway/window" ];
          modules-right = [ "cpu" "temperature" "memory" "network" "mpd" "pulseaudio" "clock" ];

          cpu = {
            interval = 1;
            states = {
              warning = 50;
              critical = 90;
            };
          };

          temperature.critical-threshold = 75;

          memory = {
            interval = 1;
            format = "{used}/{total}GB";
            states = {
              warning = 65;
              critical = 85;
            };
          };

          network = {
            interval = 1;
            format = "↑{bandwidthUpBits} ↓{bandwidthDownBits}";
          };

          mpd = rec {
            format = "{title} — {artist}";
            format-paused = format;
          };

          pulseaudio.format-muted = "0%";

          clock.format = "{:%a %d/%m %H:%M}";
        };
      };
      style = with lib; ''
        * {
          min-height: ${toString barHeight}px;
          font-family: Hasklig;
          font-size: 12px;
        }

        #waybar {
          opacity: .92;
        }

        #workspaces button {
          padding: 0;
        }

        #workspaces button.focused {
          background: #${config.colorScheme.colors.base01};
        }

        #workspaces button,
        #mode,
        #window,
        #cpu,
        #temperature,
        #memory,
        #network,
        #mpd,
        #pulseaudio,
        #clock {
          color: #${config.colorScheme.colors.base05};
        }

        #mpd.paused,
        #mpd.stopped {
          color: #${config.colorScheme.colors.base03};
        }

        #cpu.warning,
        #temperature.critical,
        #memory.warning,
        #mpd.disconnected,
        #pulseaudio.muted {
          color: #${config.colorScheme.colors.base09};
        }

        #cpu.critical,
        #memory.critical {
          color: #${config.colorScheme.colors.base08};
        }
      '';
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
        package = (nix-colors.lib-contrib { inherit pkgs; }).gtkThemeFromScheme { scheme = config.colorScheme; };
        name = config.colorScheme.slug;
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
