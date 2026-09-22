{ pkgs, ... }:

{
  programs.fish = {
    enable = true;

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
      right_format = "$directory";
      directory.style = "purple";
    };
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
          "jj:ps" = [ "git push" ];
          "jj:retrunk" = [ "rebase -d 'trunk()'" ];
          "jj:sq" = [ "squash" ];
        };
      };

  programs.bat.enable = true;

  # Suppress login shell MOTD.
  home.file.".hushlogin".text = "";

  programs.git.ignores = [
    "result"
  ];

  home.sessionPath = [
    # Rustup
    "/opt/homebrew/opt/rustup/bin"
    # GitButler
    "$HOME/.local/bin"
  ];
}
