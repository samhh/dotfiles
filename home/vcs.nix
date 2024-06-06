{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:

let
  jj-attr =
    let
      fzf = "${pkgs.fzf}/bin/fzf";
      git = "${pkgs.git}/bin/git";
      jj = "${pkgs.jujutsu}/bin/jj";
      sd = "${pkgs.sd}/bin/sd";
    in
    pkgs.writeShellScriptBin "jj-attr" ''
      set -e

      rev=''${1:-@};

      recent=$(${git} shortlog -sec --since=1.month | ${sd} '^\s*[0-9]+\s*(.+)$' '$1')
      prefix='Co-authored-by: '
      # Beware a trailing \n coming from fzf.
      msg=$(echo "$recent" | ${fzf} -m | ${sd} '^(.+)' "$prefix\$1")

      ${jj} desc "$rev" -m "$(${jj} log --no-graph -r "$rev" -T description)" -m "$msg"
    '';
  jj-cp =
    let
      jj = "${pkgs.jujutsu}/bin/jj";
      rg = "${pkgs.ripgrep}/bin/rg";
    in
    pkgs.writeShellScriptBin "jj-cp" ''
      set -e

      src="$1"
      dest=''${2:-@}

      cid=$(${jj} duplicate "$src" 2>&1 | tee /dev/stderr | ${rg} -o -r '$1' 'Duplicated \w+ as (\w+)')

      ${jj} rebase -s "$cid" -d "$dest"
    '';
  jj-review =
    let
      jj = "${pkgs.jujutsu}/bin/jj";
    in
    pkgs.writeShellScriptBin "jj-review" ''
      set -e

      branch="$1"
      remote=''${2:-origin}

      ${jj} git fetch -b "$branch"
      ${jj} new "$branch@$remote"
    '';
in
{
  programs.jujutsu = {
    enable = true;
    package = pkgs-unstable.jujutsu;
    settings = {
      user = {
        name = "Sam A. Horvath-Hunt";
        email = "hello@samhh.com";
      };
      signing = {
        sign-all = true;
        backend = "gpg";
        backends.gpg = {
          program = "${pkgs.gnupg}/bin/gpg";
          allow-expired-keys = false;
        };
        key = "4667250BD56735A8";
      };
      git.push-branch-prefix = "jj-";
      template-aliases = {
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      aliases = {
        "ab" = [ "abandon" ];
        "br" = [ "branch" ];
        "dup" = [ "duplicate" ];
        "ft" = [
          "git"
          "fetch"
        ];
        "la" = [
          "log"
          "-r"
          "anon()"
        ];
        "lah" = [
          "log"
          "-r"
          "heads(anon())"
          "--no-graph"
        ];
        "lar" = [
          "log"
          "-r"
          "roots(anon())"
          "--no-graph"
        ];
        "lb" = [
          "log"
          "-r"
          "my_branches()"
        ];
        "lbh" = [
          "log"
          "-r"
          "heads(my_branches())"
          "--no-graph"
        ];
        "lt" = [
          "log"
          "-r"
          "trunk()..@"
        ];
        "ps" = [
          "git"
          "push"
        ];
        "rb" = [ "rebase" ];
        "sp" = [ "split" ];
        "sq" = [ "squash" ];
        "ws" = [ "workspace" ];
      };
      revset-aliases = {
        "anon()" = "mine() ~ ::(branches() | remote_branches())";
        "my_branches()" = "::branches() ~ ::trunk()";
      };
    };
  };

  # See: https://github.com/nix-community/home-manager/pull/5207
  home.sessionVariables = lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin) {
    JJ_CONFIG = "${config.xdg.configHome}/jj/config.toml";
  };

  programs.git = {
    enable = true;

    delta = {
      enable = true;
      catppuccin.enable = true;
    };

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
    };

    ignores = [ ".DS_Store" ];
  };

  home.packages = with pkgs; [
    jj-attr
    jj-cp
    jj-review
    tig
  ];
}
