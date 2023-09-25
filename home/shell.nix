{ pkgs, ... }:

let
  fishMinimalTheme = rec {
    name = "fish-minimal-theme";
    src = pkgs.fetchFromGitHub {
      owner = "samhh";
      repo = name;
      rev = "f1d159b9d6b6f212f4de143afc743eef45a29533";
      sha256 = "07nfnajmvhr3908gvjgx83rfnxm5pnvri6ahybyqhs7cqpnl8h4q";
    };
  };

  fishForeignEnv = {
    name = "foreign-env";
    src = pkgs.fetchFromGitHub {
      owner = "oh-my-fish";
      repo = "plugin-foreign-env";
      rev = "dddd9213272a0ab848d474d0cbde12ad034e65bc";
      sha256 = "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs";
    };
  };

  fishCompletionSync = rec {
    name = "fish-completion-sync";
    src = pkgs.fetchFromGitHub {
      owner = "pfgray";
      repo = name;
      rev = "ba70b6457228af520751eab48430b1b995e3e0e2";
      sha256 = "sha256-JdOLsZZ1VFRv7zA2i/QEZ1eovOym/Wccn0SJyhiP9hI=";
    };
  };
in
{
  programs.fish = {
    enable = true;

    # Nota bene the incompatibililty between nixpkgs fish plugins and
    # home-manager:
    #   https://github.com/nix-community/home-manager/issues/2451
    plugins = [
      fishMinimalTheme
      fishForeignEnv
      fishCompletionSync
    ];

    shellInit = ''
      set fish_greeting
      fish_vi_key_bindings

      source ${pkgs.vimPlugins.kanagawa-nvim}/extras/kanagawa.fish

      if test -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        fenv source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
      end

      set -x VISUAL hx
      set -x EDITOR hx
      set -x DIRENV_LOG_FORMAT ""
    '';

    shellAbbrs = {
      "grep" = "rg";
      "find" = "fd";
      "tree" = "tre";
      "sed" = "sd";
      "df" = "duf";
      "du" = "gdu";
      "cat" = "bat";

      "sh" = "nix shell nixpkgs#";
      "nixh" = "nix-prefetch-url";
      "nixhu" = "nix-prefetch-url --unpack";

      "up" = "home-manager switch --flake ~/Dotfiles/";
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

  programs.zoxide.enable = true;

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    mouse = true;
  };

  # Keep the GUI Alacritty installation roughly up to date and in sync with this.
  programs.alacritty = {
    enable = true;
    settings.window.option_as_alt = "Both";
  };

  programs.git.ignores = [
    "result"
    ".envrc"
    ".direnv/"
  ];
}
