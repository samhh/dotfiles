{
  pkgs,
  pkgs-unstable,
  ...
}:

let
  name = "Sam A. Horvath-Hunt";
  email = "hello@samhh.com";
  pub-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICF3PGCLbd7QTcz4cSYONosH7tyJFsncXDTA/qRBo7/A";

  jj-trailer =
    let
      jj = "${pkgs-unstable.jujutsu}/bin/jj";
    in
    pkgs.writeShellScriptBin "jj-trailer" ''
      set -e

      key="$1";
      val="$2";
      rev=''${3:-@};

      msg="$1: $2"

      ${jj} desc "$rev" -m "$(${jj} log --no-graph -r "$rev" -T description)" -m "$msg"
    '';

  # Supported by:
  #   - GitHub: https://docs.github.com/en/pull-requests/committing-changes-to-your-project/creating-and-editing-commits/creating-a-commit-with-multiple-authors#creating-co-authored-commits-on-the-command-line
  jj-coauthor =
    let
      fzf = "${pkgs.fzf}/bin/fzf";
      git = "${pkgs.git}/bin/git";
      sd = "${pkgs.sd}/bin/sd";
      trailer = "${jj-trailer}/bin/jj-trailer";
    in
    pkgs.writeShellScriptBin "jj-coauthor" ''
      set -e

      rev="$1";

      # Beware a trailing \n coming from fzf.
      coauthor=$(${git} shortlog -sec --since=1.month | ${sd} '^\s*[0-9]+\s*(.+)$' '$1' | ${fzf} -m)

      ${trailer} Co-authored-by "$coauthor" "$rev"
    '';

  jj-review =
    let
      jj = "${pkgs-unstable.jujutsu}/bin/jj";
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
        inherit name email;
      };
      signing = {
        sign-all = true;
        backend = "ssh";
        key = pub-key;
      };
      git.push-bookmark-prefix = "jj-";
      template-aliases = {
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      aliases = {
        "ab" = [ "abandon" ];
        "bm" = [ "bookmark" ];
        "co" = [
          "util"
          "exec"
          "--"
          "${jj-coauthor}/bin/jj-coauthor"
        ];
        "dp" = [ "duplicate" ];
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
          "my_bookmarks()"
        ];
        "lbh" = [
          "log"
          "-r"
          "heads(my_bookmarks())"
          "--no-graph"
        ];
        "lt" = [
          "log"
          "-r"
          "(trunk()..@)::"
        ];
        "plb" = [
          "bookmark"
          "move"
          "--from"
          "heads(::@- & bookmarks()) ~ trunk()"
          "--to"
          "@-"
        ];
        "plt" = [
          "bookmark"
          "move"
          "--from"
          "trunk()"
          "--to"
          "@-"
        ];
        "ps" = [
          "git"
          "push"
        ];
        "rb" = [ "rebase" ];
        "rv" = [
          "util"
          "exec"
          "--"
          "${jj-review}/bin/jj-review"
        ];
        "sp" = [ "split" ];
        "sq" = [ "squash" ];
        "ws" = [ "workspace" ];
      };
      revset-aliases = {
        "anon()" = "mine() ~ (null() & heads(::)) ~ ::(bookmarks() | remote_bookmarks())";
        "my_bookmarks()" = "::bookmarks() ~ ::trunk()";
        "null()" = "empty() & description(exact:'')";
      };
      # Until there's a programs.jujutsu.delta.enable option:
      #   https://github.com/nix-community/home-manager/issues/4887
      ui = {
        pager = "${pkgs.delta}/bin/delta";
        diff.format = "git";
      };
    };
  };

  programs.git = {
    enable = true;

    extraConfig = {
      user = {
        inherit name email;
        signingkey = pub-key;
      };
      gpg = {
        format = "ssh";
        ssh.allowedSignersFile = builtins.toFile "allowed_signers" ''
          ${email} ${pub-key}
        '';
      };
      tag.gpgSign = true;
      url = {
        "git@git.sr.ht:~".insteadOf = "sh:";
        "git@github.com:".insteadOf = "gh:";
      };
      init.defaultBranch = "trunk";
    };

    ignores = [ ".DS_Store" ];
  };

  home.packages = with pkgs; [
    tig
  ];
}
