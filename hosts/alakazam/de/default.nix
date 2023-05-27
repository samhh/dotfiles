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

  locker = import ./locker.nix { inherit pkgs; };
  wallpaper = import ./wallpaper.nix { inherit config lib pkgs; };
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
        mprisPlayers = lib.concatStringsSep "," [ "mpv" "firefox" "cider" "mpd" ];
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

          input =
            let
              wiredTrackpadId = "1452:613:Apple_Inc._Magic_Trackpad";
              wirelessTrackpadId = "76:613:Apple_Inc._Magic_Trackpad_2";
              cfg = {
                natural_scroll = "enabled";
                scroll_factor = "0.3";
              };
            in
            {
              ${wiredTrackpadId} = cfg;
              ${wirelessTrackpadId} = cfg;
            };

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
            "${mod}+d" = "exec ${scripts}/flatmarks.sh ~/bookmarks/";
            "${mod}+Shift+d" = "exec ${scripts}/flatmarks.sh ~/bookmarks-work/";
            # Quick access clipboard is currently broken on Sway:
            #   https://www.reddit.com/r/1Password/comments/yq32ar/unable_to_copy_from_quick_access_in_linux/
            "${mod}+x" = "exec ${pkgs._1password-gui}/bin/1password --quick-access";
            "${mod}+c" = "exec ${pkgs._1password-gui}/bin/1password --toggle";
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

        extraConfig = ''
          bindgesture swipe:3:left workspace next
          bindgesture swipe:3:right workspace prev
        '';
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
          modules-right = [ "cpu" "memory" "network" "tray" "mpd" "pulseaudio" "clock" ];

          cpu = {
            interval = 1;
            states = {
              warning = 50;
              critical = 90;
            };
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
          border-radius: 0;
        }

        #workspaces button.focused {
          background: #${config.colorScheme.colors.base01};
        }

        #cpu,
        #memory,
        #network,
        #mpd,
        #pulseaudio,
        #clock {
          padding: 0 8px;
        }

        #workspaces button,
        #mode,
        #window,
        #cpu,
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

    services.mako = {
      enable = true;
      backgroundColor = "#${config.colorScheme.colors.base00}${bgOpacityHex}";
      borderSize = 0;
      inherit borderRadius;
      padding = "${toString vPadding},${toString hPadding}";
    };

    systemd.user.services = {
      # Needed by 1Password. From:
      #   https://github.com/nix-community/home-manager/issues/3739#issue-1609877168
      sway-polkit-authentication-agent = {
        Unit = {
          Description = "Sway Polkit authentication agent";
          Documentation = "https://gitlab.freedesktop.org/polkit/polkit/";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "always";
        };

        Install.WantedBy = [ "graphical-session.target" ];
      };

      wallpaper = {
        Install.WantedBy = [ wmTarget ];
        Service = {
          ExecStart = "${wallpaper}/bin/wallpaper";
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
        package = (nix-colors.lib.contrib { inherit pkgs; }).gtkThemeFromScheme { scheme = config.colorScheme; };
        name = config.colorScheme.slug;
      };
    };

    home.packages = with pkgs; [
      emote
      sway-contrib.grimshot

      # For various scripts.
      bash
      tofi

      # For scripts interacting with `swaymsg`.
      gron
      jq

      # For backup script.
      tree
    ];
  };
}
