{ config, lib, nix-colors, pkgs, ... }:

let
  scripts = "${config.users.users.${config.username}.home}/dotfiles/hosts/alakazam/scripts";
  output = "DP-3";
  wmTarget = "sway-session.target";
  barName = "top";
  res = { w = 2560; h = 1440; r = 240; };
  # It's approx because Waybar won't respect it if the contents need to exceed
  # it.
  approxBarHeight = 24;
  gap = 10;
  bgOpacityPerc = 92;
  # The same as the percentage. Ideally we'd calculate this here in Nix.
  bgOpacityHex = "EB";
  vPadding = 10;
  hPadding = builtins.floor (vPadding * 1.5);
  borderRadius = 2;

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
          bars = [ ];
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
                pos = {
                  w = res.w - win.w - spacing;
                  h = res.h - win.h - spacing - approxBarHeight;
                };
              in
              "floating enable; sticky enable; border none; resize set ${toString win.w} ${toString win.h}; move position ${toString pos.w} ${toString pos.h}";
          }];
        };
      };

    programs.waybar = {
      enable = true;
      systemd = {
        enable = true;
        target = wmTarget;
      };
      settings = {
        ${barName} = {
          position = "top";
          height = approxBarHeight;

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

          temperature = {
            interval = 1;
            # Might not be stable but at time of writing this appears to
            # correspond to the output of `sensors k10temp-pci-00c3`.
            hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
            critical-threshold = 75;
          };

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
          min-height: 0;
          font-family: Hasklig;
          font-size: 12px;
        }

        #waybar {
          opacity: 0.${toString bgOpacityPerc};
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

        #workspaces button.urgent,
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

    programs.mako = {
      enable = true;
      backgroundColor = "#${config.colorScheme.colors.base00}${bgOpacityHex}";
      borderSize = 0;
      inherit borderRadius;
      padding = "${toString vPadding},${toString hPadding}";
    };

    systemd.user.services = {
      # Needs to be defined explicitly, see:
      #   https://github.com/nix-community/home-manager/issues/2028#issuecomment-1231564220
      mako = {
        Install.WantedBy = [ wmTarget ];
        Service.ExecStart = "${pkgs.mako}/bin/mako";
      };

      wallpaper = {
        Install.WantedBy = [ wmTarget ];
        Service = {
          ExecStart = "${scripts}/set-rand-wallpaper.sh ${config.nas.path}/bgs";
          Environment =
            let deps = with pkgs; [ coreutils findutils swaybg ];
            in [ "PATH=${lib.makeBinPath deps}" ];
          Restart = "always";
          RuntimeMaxSec = "3h";
        };
      };
    };

    xdg.configFile."tofi/config".text = ''
      fuzzy-match = true

      width = 425
      height = 200
      font-size = 10
      font = monospace
      outline-width = 0
      border-width = 0
      corner-radius = ${toString borderRadius}
      result-spacing = ${toString (vPadding / 2)}
      padding-top = ${toString vPadding}
      padding-bottom = ${toString vPadding}
      padding-left = ${toString hPadding}
      padding-right = ${toString hPadding}
      prompt-padding = ${toString hPadding}

      background-color = ${config.colorScheme.colors.base00}${bgOpacityHex}
      text-color       = ${config.colorScheme.colors.base05}
      selection-color  = ${config.colorScheme.colors.base09}
    '';

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