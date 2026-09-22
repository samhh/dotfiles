{ ... }:

{
  programs.fish = {
    enable = true;

    shellInit = ''
      set -g fish_greeting

      ssh-add 2> /dev/null &
      source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish

      if not set -q VISUAL; set -x VISUAL hx; end
    '';

    shellAbbrs = {
      nn = "jj";
      nnui = "jjui";
    };

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
