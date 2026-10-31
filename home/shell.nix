{ ... }:

{
  programs.fish = {
    enable = true;

    shellInit = ''
      set -g fish_greeting

      ssh-add 2> /dev/null &
      source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
      brew shellenv fish | source

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
      right_format = "$vcs$directory";
      directory.style = "purple";
      # Prefer jj in colocated repos, else git would win.
      vcs.order = [
        "jj"
        "git"
      ];
      jj_bookmark.format = "[$bookmark(@$remote)$diverged]($style) ";
      # Agents push branches that would otherwise crowd out local bookmarks.
      jj_bookmark.ignore_remotes = [
        "origin"
        "git"
      ];
      git_branch.format = "[$branch(:$remote_branch)]($style) ";
      # Shortest unique prefix only.
      jj_change.change_hash_length = 1;
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
