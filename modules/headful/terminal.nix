{ config, lib, pkgs, uname, webBrowserBin, ... }:

let
  uid = config.users.users.${uname}.uid;
  home = config.users.users.${uname}.home;
  editorBin = "${config.home-manager.users.${uname}.programs.neovim.finalPackage}/bin/nvim";

  # Workaround for incompatibililty between nixpkgs fish plugins and
  # home-manager:
  #   https://github.com/nix-community/home-manager/issues/2451
  #
  # Can't use `pkgs.wrapFish` directly as it drops everything except the
  # binary, and Home Manager relies upon other stuff in the package such
  # as a script for completions generation.
  customFish =
    pkgs.fish.overrideAttrs (attrs: {
      fixupPhase =
        let
          overrides = {
            pluginPkgs = with pkgs.fishPlugins; [
              fish-minimal-theme
              z
            ];
          };
        in
        ''
          cp ${pkgs.wrapFish overrides}/bin/fish $out/bin/fish
        '';
    });

in
lib.mkMerge [
  {
    users.users.${uname}.shell = customFish;

    security.doas.extraRules = [{
      users = [ uname ];
      keepEnv = true;
    }];

    home-manager.users.${uname} = {
      programs.fish = {
        enable = true;
        package = customFish;
        shellInit = ''
          set fish_greeting
          fish_vi_key_bindings

          source ${pkgs.vimPlugins.kanagawa-nvim}/extras/kanagawa.fish

          set -x VISUAL ${editorBin}
          set -x EDITOR ${editorBin}
          set -x MANPAGER ${editorBin} +Man!
          set -x DIFFPROG ${editorBin} -d
          set -x DIRENV_LOG_FORMAT ""
        '';
        shellAbbrs = {
          "diff" = "nvim -d";
          "grep" = "rg";
          "vi" = "nvim";

          "sh" = "nix shell";
          "nixu" = "nix-prefetch-url --unpack";
        };
        functions = {
          mkcd = "mkdir -p $argv; cd $argv;";
          mktouch = "mkdir -p (dirname $argv); touch $argv;";
        };
      };

      programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      programs.git.ignores = [
        "shell.nix"
        "result"
        ".envrc"
        ".direnv/"
      ];
    };
  }

  (lib.mkIf pkgs.stdenv.isDarwin {
    home-manager.users.${uname}.programs.fish = {
      shellInit = ''
        set fish_function_path $fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d
        fenv source ${config.system.build.setEnvironment}
      '';

      shellAbbrs = {
        "up" = "darwin-rebuild build";
        "upp" = "sudo darwin-rebuild switch";
      };
    };
  })

  (lib.mkIf pkgs.stdenv.isLinux {
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
  })
]
