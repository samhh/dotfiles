{
  lib,
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
      jj = lib.getExe pkgs-unstable.jujutsu;
    in
    pkgs.writeFishScript "jj-trailer" ''
      argparse -N 2 'r/revisions=' -- $argv; or exit $status

      set -l rev $_flag_revisions; and if test -z $rev; set rev @; end
      set -l key $argv[1]
      set -l vals $argv[2..]

      set -l trailers

      for i in (seq (count $vals))
        set trailers[$i] "$key: $vals[$i]"
      end

      for commit in (${jj} log --no-graph -r $rev -T 'commit_id ++ "\n"')
        set -l prev (${jj} log --no-graph -r $commit -T description)
        ${jj} desc $commit -m "$prev" -m "$(string join \n $trailers)"
      end
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
        pager = lib.getExe pkgs.delta;
        diff.format = "git";
        show-cryptographic-signatures = true;
        default-command = "log";
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
        "ft" = [
          "git"
          "fetch"
        ];
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

        "review" =
          let
            jj-review =
              let
                jj = lib.getExe pkgs-unstable.jujutsu;
              in
              pkgs.writeFishScript "jj-review" ''
                argparse -i 'remote=' -- $argv; or exit $status

                set -l remote $_flag_remote; and if test -z $remote; set remote origin; end
                set -l branch $argv[1]
                set -l args $argv[2..]

                ${jj} git fetch -b $branch
                ${jj} new "$branch@$remote" $args
              '';
          in
          [
            "util"
            "exec"
            "--"
            jj-review
          ];

        "trailer" = [
          "util"
          "exec"
          "--"
          jj-trailer
        ];

        # Supported by:
        #   - Sourcehut: https://man.sr.ht/git.sr.ht/#closes
        #   - GitHub: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword
        "closes" = [
          "trailer"
          "Closes"
        ];

        # Supported by:
        #   - Sourcehut: https://man.sr.ht/git.sr.ht/#fixes
        #   - GitHub: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword
        "fixes" = [
          "trailer"
          "Fixes"
        ];

        # Supported by:
        #   - GitHub: https://docs.github.com/en/actions/managing-workflow-runs-and-deployments/managing-workflow-runs/skipping-workflow-runs
        "skipchecks" = [
          "trailer"
          "skip-checks"
          "true"
        ];

        # Supported by:
        #   - GitHub: https://docs.github.com/en/pull-requests/committing-changes-to-your-project/creating-and-editing-commits/creating-a-commit-with-multiple-authors#creating-co-authored-commits-on-the-command-line
        "coauthor" =
          let
            jj-coauthor =
              let
                fzf = lib.getExe pkgs.fzf;
                git = lib.getExe pkgs.git;
                sd = lib.getExe pkgs.sd;
              in
              pkgs.writeFishScript "jj-coauthor" ''
                argparse -i 'r/revisions=' -- $argv; or exit $status

                set -l rev $_flag_revisions; and if test -z $rev; set rev @; end
                set -l fzf_args $argv

                set -l coauthors (${git} shortlog -sec --since=1.month | ${sd} '^\s*[0-9]+\s*(.+)$' '$1' | ${fzf} -m $fzf_args)

                jj trailer -r $rev Co-authored-by $coauthors
              '';
          in
          [
            "util"
            "exec"
            "--"
            jj-coauthor
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
