{ pkgs, ... }:

let
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
      fishCompletionSync
    ];

    shellInit = ''
      set -g fish_greeting

      source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish

      ssh-add 2> /dev/null &

      if not set -q VISUAL; set -x VISUAL hx; end
    '';

    shellAbbrs = {
      "cat" = "bat";
      "nn" = "jj";

      "sh" = "nix shell nixpkgs#";
      "nixh" = "nix-prefetch-url";
      "nixhu" = "nix-prefetch-url --unpack";

      "up" = "home-manager switch --flake ~/Dev/dotfiles/";
    };

    functions = {
      mkcd = "mkdir -p $argv; cd $argv;";
      mktouch = "mkdir -p (dirname $argv); touch $argv;";
      touchx = "touch $argv; chmod +x $argv";
    };
  };

  programs.starship = {
    enable = true;
    settings = {
      scan_timeout = 5;

      character = {
        success_symbol = "λ";
        error_symbol = "!";
      };
      format = "$character";
      right_format = "$direnv$nix_shell$directory";

      direnv = {
        disabled = false;
        format = "[$allowed]($style)";
        style = "red";
        allowed_msg = "";
        not_allowed_msg = "? ";
        denied_msg = " ";
      };
      nix_shell = {
        format = "[$symbol]($style)";
        symbol = " ";
      };
      directory.style = "purple";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config.global = {
      hide_env_diff = true;
      warn_timeout = 0;
    };
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

  # Suppress login shell MOTD.
  home.file.".hushlogin".text = "";

  programs.git.ignores = [
    "result"
    ".envrc"
  ];

  home.packages = with pkgs; [
    nerd-fonts.fira-code
  ];
}
