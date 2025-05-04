{
  pkgs,
  pkgs-unstable,
  ...
}:

let
  name = "Sam A. Horvath-Hunt";
  email = "hello@samhh.com";
  pub-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICF3PGCLbd7QTcz4cSYONosH7tyJFsncXDTA/qRBo7/A";
  allowed-signers = builtins.toFile "allowed-signers" ''
    ${email} ${pub-key}
  '';

  jj-trailer =
    let
      jj = "${pkgs-unstable.jujutsu}/bin/jj";
    in
    pkgs.writeShellScriptBin "jj-trailer" ''
      set -e

      key="$1"
      val="$2"
      rev=''${3:-@}

      msg=
      readarray -t vals <<< "$val"
      for val in "''${vals[@]}"; do
        if [ -n "$msg" ]; then msg+=$'\n'; fi
        msg+="$key: $val"
      done

      for commit in ''$(${jj} log --no-graph -r "$rev" -T 'commit_id ++ "\n"'); do
        ${jj} desc "$commit" -m "$(${jj} log --no-graph -r "$commit" -T description)" -m "$msg"
      done
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
        behavior = "own";
        backend = "ssh";
        backends.ssh = {
          inherit allowed-signers;
        };
        key = pub-key;
      };
      git.push-bookmark-prefix = "jj-";
      ui = {
        # Until there's a programs.jujutsu.delta.enable option:
        #   https://github.com/nix-community/home-manager/issues/4887
        pager = "${pkgs.delta}/bin/delta";
        diff.format = "git";
        show-cryptographic-signatures = true;
      };
      revsets.log = "(trunk()..@)::";
      template-aliases = {
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      revset-aliases = {
        "anon()" = "mine() ~ (null() & heads(::)) ~ ::(bookmarks() | remote_bookmarks())";
        "null()" = "empty() & description(exact:'')";
      };
      aliases = {
        "ab" = [ "abandon" ];
        "bm" = [ "bookmark" ];
        "ft" = [
          "git"
          "fetch"
        ];
        "lg" = [ "log" ];
        "ps" = [
          "git"
          "push"
        ];
        "sq" = [ "squash" ];

        "loga" = [
          "log"
          "-r"
          "anon()"
        ];
        "newt" = [
          "new"
          "trunk()"
        ];
        "rbt" = [
          "rebase"
          "-d"
          "trunk()"
        ];
        "tug" = [
          "bookmark"
          "move"
          "-f"
          "heads(::@ & bookmarks()) ~ trunk()"
          "-t"
          "heads(::@ & mutable() ~ null())"
        ];
        "tugt" = [
          "bookmark"
          "move"
          "-f"
          "trunk()"
          "-t"
          "heads(::@ & mutable() ~ null())"
        ];

        # Supported by:
        #   - Sourcehut: https://man.sr.ht/git.sr.ht/#closes
        #   - GitHub: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword
        "closes" =
          let
            jj-closes =
              let
                trailer = "${jj-trailer}/bin/jj-trailer";
              in
              pkgs.writeShellScriptBin "jj-closes" ''
                set -e

                url="$1"
                rev="$2"

                ${trailer} Closes "$url" "$rev"
              '';
          in
          [
            "util"
            "exec"
            "--"
            "${jj-closes}/bin/jj-closes"
          ];

        # Supported by:
        #   - GitHub: https://docs.github.com/en/pull-requests/committing-changes-to-your-project/creating-and-editing-commits/creating-a-commit-with-multiple-authors#creating-co-authored-commits-on-the-command-line
        "coauthor" =
          let
            jj-coauthor =
              let
                fzf = "${pkgs.fzf}/bin/fzf";
                git = "${pkgs.git}/bin/git";
                sd = "${pkgs.sd}/bin/sd";
                trailer = "${jj-trailer}/bin/jj-trailer";
              in
              pkgs.writeShellScriptBin "jj-coauthor" ''
                set -e

                rev="$1"
                args=''${@:2}

                coauthors=$(${git} shortlog -sec --since=1.month | ${sd} '^\s*[0-9]+\s*(.+)$' '$1' | ${fzf} -m "$args")

                ${trailer} Co-authored-by "$coauthors" "$rev"
              '';
          in
          [
            "util"
            "exec"
            "--"
            "${jj-coauthor}/bin/jj-coauthor"
          ];

        # Supported by:
        #   - Sourcehut: https://man.sr.ht/git.sr.ht/#fixes
        #   - GitHub: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword
        "fixes" =
          let
            jj-fixes =
              let
                trailer = "${jj-trailer}/bin/jj-trailer";
              in
              pkgs.writeShellScriptBin "jj-fixes" ''
                set -e

                url="$1"
                rev="$2"

                ${trailer} Fixes "$url" "$rev"
              '';
          in
          [
            "util"
            "exec"
            "--"
            "${jj-fixes}/bin/jj-fixes"
          ];

        "review" =
          let
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
          [
            "util"
            "exec"
            "--"
            "${jj-review}/bin/jj-review"
          ];

        # Supported by:
        #   - GitHub: https://docs.github.com/en/actions/managing-workflow-runs-and-deployments/managing-workflow-runs/skipping-workflow-runs
        "skipchecks" =
          let
            jj-skipchecks =
              let
                trailer = "${jj-trailer}/bin/jj-trailer";
              in
              pkgs.writeShellScriptBin "jj-skipchecks" ''
                set -e

                rev="$1"

                ${trailer} skip-checks true "$rev"
              '';
          in
          [
            "util"
            "exec"
            "--"
            "${jj-skipchecks}/bin/jj-skipchecks"
          ];
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
        ssh.allowedSignersFile = allowed-signers;
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
    # For verifying others' commit signatures.
    gnupg
    tig
  ];
}
