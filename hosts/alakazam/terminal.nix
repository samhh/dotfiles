# NB I'd recommend not calling this file shell.nix as that's globally ignored
# in vcs.nix, and it's easy to forget to `git add -f`...

{ config, pkgs, uname, webBrowserBin, ... }:

let
  uid = config.users.users.${uname}.uid;
  home = config.users.users.${uname}.home;
  editorBin = "${config.home-manager.users.${uname}.programs.neovim.finalPackage}/bin/nvim";

  fish-minimal-theme = {
    name = "fish-minimal-theme";
    src = builtins.fetchTarball {
      url = "https://github.com/samhh/fish-minimal-theme/archive/9cabe0f044bb80bcbfec7d6804971836003df681.tar.gz";
      sha256 = "1ilmnjxsaqzkjlqdn2m348bfjg23k6dkcak5p7qb84yz13pf3dfv";
    };
  };

  z = {
    name = "z";
    src = builtins.fetchTarball {
      url = "https://github.com/jethrokuan/z/archive/45a9ff6d0932b0e9835cbeb60b9794ba706eef10.tar.gz";
      sha256 = "1kjyl4gx26q8175wcizvsm0jwhppd00rixdcr1p7gifw6s308sd5";
    };
  };

in {
  users.users.${uname}.shell = pkgs.fish;

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
      enable = true;
      shellInit = ''
        set fish_greeting
        fish_vi_key_bindings

        source ${pkgs.vimPlugins.kanagawa-nvim}/extras/kanagawa.fish

        set -x VISUAL ${editorBin}
        set -x EDITOR ${editorBin}
        set -x MANPAGER ${editorBin} +Man!
        set -x DIFFPROG ${editorBin} -d
        set -x SSH_AUTH_SOCK /run/user/${toString uid}/ssh-agent
        set -x DIRENV_LOG_FORMAT ""
      '';
      shellAbbrs = {
        "mann" = "tldr";
        "diff" = "nvim -d";
        "grep" = "rg";
        "find" = "fd";
        "tree" = "tre";
        "sed" = "sd";
        "df" = "duf";
        "du" = "gdu";
        "ping" = "gping";
        "mpc" = "vimpc";
        "sys" = "systemctl";
        "sysu" = "systemctl --user";
        "top" = "gotop";
        "vi" = "nvim";
        "up" = "nixos-rebuild build";
        "upp" = "doas nixos-rebuild switch";
        "sh" = "nix shell";
        "nixu" = "nix-prefetch-url --unpack";
      };
      functions = {
        mkcd = "mkdir -p $argv; cd $argv;";
        mktouch = "mkdir -p (dirname $argv); touch $argv;";
      };
      plugins = [
        fish-minimal-theme
        z
      ];
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
