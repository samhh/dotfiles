# NB I'd recommend not calling this file shell.nix as that's globally ignored
# in vcs.nix, and it's easy to forget to `git add -f`...

{ config, pkgs, uname, editorBin, webBrowserBin, ... }:

let
  uid = config.users.users.${uname}.uid;
  home = config.users.users.${uname}.home;

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
        colors.alpha = .92;
      };
    };

    programs.fish = {
      enable = true;
      shellInit = ''
        set fish_greeting
        fish_vi_key_bindings

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
      ".envrc"
      ".direnv/"
    ];

    programs.tmux = {
      enable = true;
      prefix = "C-z";
    };

    home.packages = with pkgs; [
      tmate
    ];
  };
}
