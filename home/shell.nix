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

      ssh-add 2> /dev/null &
      source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
      am init fish | source

      if not set -q VISUAL; set -x VISUAL hx; end
    '';

    functions = {
      mkcd = "mkdir -p $argv; cd $argv;";
      mktouch = "mkdir -p (dirname $argv); touch $argv;";
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
      right_format = "$direnv$nix_shell$mise$directory";

      direnv = {
        disabled = false;
        format = "[$allowed]($style)";
        style = "blue";
        allowed_msg = "";
        not_allowed_msg = "? ";
        denied_msg = "";
      };
      nix_shell = {
        format = "[$symbol]($style)";
        symbol = " ";
      };
      mise = {
        disabled = false;
        format = "[$symbol]($style)";
        symbol = " ";
        style = "red";
      };
      directory.style = "purple";
    };
  };

  programs.mise.enable = true;

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

  xdg.configFile."amoxide/config.toml".source =
    (pkgs.formats.toml { }).generate "amoxide/config.toml"
      {
        shell.fish.use_abbr = true;

        aliases = {
          nn = "jj";
          nnui = "jjui";
        };

        subcommands = {
          "jj:ab" = [ "abandon" ];
          "jj:anon" = [ "log -r 'anon()'" ];
          "jj:ft" = [ "git fetch" ];
          "jj:merge" = [ "new --no-edit -m 'Merge trunk' -B 'mega()' -A 'trunk()' -A" ];
          "jj:ps" = [ "git push" ];
          "jj:remega" = [ "rebase -s 'mega()' -d 'mega()-' -d 'trunk()' --simplify-parents" ];
          "jj:retrunk" = [ "rebase -d 'trunk()'" ];
          "jj:sq" = [ "squash" ];
          "jj:sub" = [ "rebase -B 'mega()' -A 'trunk()'" ];
          "jj:toggle" = [ "rebase -s 'mega()' -d 'toggle({{1}})'" ];
          "jj:top" = [ "squash -B 'mega()' -A 'latest(trunk()..mega()-)'" ];
        };
      };

  programs.bat.enable = true;

  # Suppress login shell MOTD.
  home.file.".hushlogin".text = "";

  programs.git.ignores = [
    "result"
    ".envrc"
  ];

  programs.npm = {
    enable = true;
    settings = {
      # Support npm i -g.
      prefix = "\${HOME}/.npm";
      # Suppress funding message.
      fund = false;
    };
  };

  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.npm/bin"
  ];

  home.packages = with pkgs; [
    amoxide
    ast-grep
    curl
    fd
    nerd-fonts.fira-code
    gdu
    gh
    nodejs
    ripgrep
    sd
    tre-command
  ];
}
