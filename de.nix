{ config, lib, pkgs, ... }:

let
  uname = "sam";
  home = config.users.users.${uname}.home;
  output = "DP-3";
  barName = "top";
in {
  fonts.fonts = with pkgs; [
    hasklig
    noto-fonts-emoji
  ];

  # Autostart, but only in tty1.
  environment.loginShellInit = ''
    if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
      exec sway
    fi
  '';

  programs.sway.enable = true;
  xdg.portal.wlr.enable = true;

  home-manager.users.${uname} = {
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
            bg = "/mnt/nas/bgs/aenami_025.jpg fill";
          };
          bars = [{
            statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-${barName}.toml";
            position = "top";
          }];
          gaps.inner = 10;
          modifier = mod;
          # Scripts aren't imported into Nix as they have relatively-pathed
          # dependencies upon other scripts Nix that doesn't know about.
          # Ideally the likes of bemenu would be packaged up in there as well
          # and not exposed in $PATH.
          keybindings = mkOptionDefault {
            "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -1%";
            "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +1%";
            "XF86AudioPlay" = "exec playerctl play-pause";
            "XF86AudioPrev" = "exec playerctl previous";
            "XF86AudioNext" = "exec playerctl next";
            "${mod}+Return" = "exec ${pkgs.foot}/bin/foot";
            "${mod}+t" = "exec ${home}/dotfiles/scripts/web-search.sh";
            "${mod}+g" = "exec ${home}/dotfiles/scripts/apps.sh";
            "${mod}+Shift+g" = "exec bemenu-run -p gui-all";
            "${mod}+d" = "exec ${home}/dotfiles/scripts/flatmarks.sh";
            "${mod}+Shift+d" = "exec ${home}/dotfiles/scripts/flatmarks-work.sh";
            "${mod}+x" = "exec ${home}/dotfiles/scripts/passmenu.sh";
            "${mod}+n" = "exec ${home}/dotfiles/scripts/pass-prefixed-line.sh \"username: \" username";
            "${mod}+m" = "exec ${home}/dotfiles/scripts/pass-prefixed-line.sh \"email: \" email";
            "${mod}+o" = "exec makoctl dismiss";
            "${mod}+Shift+o" = "exec makoctl dismiss -a";
            "${mod}+p" = "exec grimshot save area";
            "${mod}+l" = "exec swaylock";
            "${mod}+Shift+l" = "exec systemctl suspend";
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
        command = "swaylock -f";
      }];
      timeouts = [{
        timeout = 1800;
        command = "swaylock -f";
      }];
    };

    services.gammastep = {
      enable = true;
      # Roughly London.
      latitude = 51.5941;
      longitude = 0.1298;
    };

    home.packages = with pkgs; [
      # For some scripts.
      bash
      bemenu
      # For scripts interacting with `swaymsg`.
      jq
      mako
      swaylock
    ];
  };
}
