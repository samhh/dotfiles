{ pkgs, ... }:

let
  fishMinimalTheme = rec {
    name = "fish-minimal-theme";
    src = pkgs.fetchFromGitHub {
      owner = "samhh";
      repo = name;
      rev = "fd543ceffd883085992cdb5102c527b90f0a63e0";
      sha256 = "sha256-uJj6hYD8RZSEkBFHg4LUznsGJyw1VCETAkFHOWg081M=";
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
    catppuccin.enable = true;

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

      if test -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        fenv source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
      end

      set -x VISUAL hx
      set -x EDITOR hx
      set -x DIRENV_LOG_FORMAT ""
    '';

    shellAbbrs = {
      "du" = "gdu";
      "cat" = "bat";
      "nn" = "jj";
      "nn-attr" = "jj-attr";
      "nn-cp" = "jj-cp";
      "nn-rv" = "jj-review";

      "sh" = "nix shell nixpkgs#";
      "nixh" = "nix-prefetch-url";
      "nixhu" = "nix-prefetch-url --unpack";

      "up" = "home-manager switch --flake ~/Dotfiles/";
    };

    functions = {
      mkcd = "mkdir -p $argv; cd $argv;";
      mktouch = "mkdir -p (dirname $argv); touch $argv;";
      touchx = "touch $argv; chmod +x $argv";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    # Avoid cluttering project directories which often conflicts with tooling,
    # as per:
    #   https://github.com/direnv/direnv/wiki/Customizing-cache-location
    stdlib = ''
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
      	echo "''${direnv_layout_dirs[$PWD]:=$(
      		echo -n "$XDG_CACHE_HOME"/direnv/layouts/
      		echo -n "$PWD" | ${pkgs.coreutils}/bin/sha1sum | cut -d ' ' -f 1
      	)}"
      }
    '';
  };

  programs.zoxide.enable = true;

  programs.git.ignores = [
    "result"
    ".envrc"
  ];
}
