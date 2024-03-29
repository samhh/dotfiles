{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    delta.enable = true;

    extraConfig = {
      push.default = "simple";
      pull.ff = "only";
      user = {
        name = "Sam A. Horvath-Hunt";
        email = "hello@samhh.com";
        signingkey = "4667250BD56735A8";
      };
      commit.gpgSign = true;
      tag.gpgSign = true;
      url = {
        "git@git.sr.ht:~".insteadOf = "sh:";
        "git@github.com:".insteadOf = "gh:";
        "ssh://aur@aur.archlinux.org/".insteadOf = "aur:";
      };
      init.defaultBranch = "master";
      merge.tool = "vimdiff";
      mergetool = {
        vimdiff.path = "nvim";
        keepBackup = false;
      };
      blame.date = "short";
      interactive.singleKey = true;
    };

    aliases = {
      attr =
        let
          attr = pkgs.writeShellScript "git-attr" ''
            set -e
            repo="''${1:-.}"
            recent=$(cd "$repo" && git shortlog -sec --since=1.month | ${pkgs.sd}/bin/sd '^\s*[0-9]+\s*(.+)$' '$1')
            prefix='Co-authored-by: '
            # Beware a trailing \n coming from fzf.
            msg=$(echo "$recent" | ${pkgs.fzf}/bin/fzf -m | sd '^(.+)' "$prefix\$1")
            git commit --amend --only -m "$(git log --format=%B -n1)" -m "$msg"
          '';
        in
        "!${attr}";
      br = "branch";
      brd = "branch -D @{-1}";
      df = "diff";
      dfs = "diff --staged";
      ca = "commit --amend --no-edit";
      cam = "commit --amend --only";
      cm = "commit";
      cma = "commit -a";
      cmx = "commit --fixup";
      cp = "cherry-pick";
      ft = "fetch";
      ignore = "!sh -c 'git add -N $1 && git update-index --skip-worktree $1' sh";
      lg = "log --graph --pretty=format:'%Cblue%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold red)[%an]%Creset' --abbrev-commit --date=relative";
      pl = "pull";
      ps = "push";
      psf = "push --force-with-lease";
      rb = "rebase";
      rbx = "rebase -i --autosquash";
      root = "rev-parse --show-toplevel";
      rs = "restore";
      rss = "restore --staged";
      rx = "reset";
      rxh = "reset --hard";
      sh = "stash";
      st = "status --short";
      sw = "switch";
      sw-gh-pr = "!sh -c 'git ft origin pull/$1/head:pr/$1 && git sw pr/$1' sh";
      wt = "worktree";
    };

    ignores = [
      ".DS_Store"
    ];
  };

  home.packages = with pkgs; [
    git-absorb
    tig
  ];
}
