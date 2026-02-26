{
  lib,
  pkgs,
  ...
}:

let
  name = "Sam A. Horvath-Hunt";
  email = "hello@samhh.com";
  pub-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICF3PGCLbd7QTcz4cSYONosH7tyJFsncXDTA/qRBo7/A";
  allowed-signers = builtins.toFile "allowed-signers" ''
    ${email} ${pub-key}
  '';

  jj = lib.getExe pkgs.jujutsu;
  jj-trailer = pkgs.writeFishScript "jj-trailer" ''
    argparse -N 2 'r/revisions=' -- $argv; or exit $status

    set -l rev $_flag_revisions; and if test -z $rev; set rev @; end
    set -l key $argv[1]
    set -l vals $argv[2..]

    set -l trailers

    for i in (seq (count $vals))
      set trailers[$i] "$key: $vals[$i]"
    end

    for commit in (${jj} log --no-graph -r $rev -T 'commit_id ++ "\n"')
      set -l prev (${jj} log --no-graph -r $commit -T description | string collect)
      ${jj} desc $commit -m "$prev" -m "$(string join \n $trailers)"
    end
  '';
in
{
  programs.jujutsu = {
    enable = true;
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
      templates.git_push_bookmark = "\"samhh/\" ++ change_id.shortest(3)";
      ui = {
        show-cryptographic-signatures = true;
        default-command = [
          "log"
          "-r"
          "here()"
        ];
      };
      template-aliases = {
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      # The stack() idea comes from:
      #   https://gist.github.com/thoughtpolice/8f2fd36ae17cd11b8e7bd93a70e31ad6
      #   https://andre.arko.net/2025/09/28/stupid-jj-tricks/#revsets
      revset-aliases = {
        "anon()" = "stack(mine() ~ ::remote_bookmarks(), 1)";
        "here()" = "stack(@, 1)";
        # https://github.com/jj-vcs/jj/discussions/7588#discussioncomment-14832469
        "mega()" = "heads(merges() & ::@)";
        "null()" = "empty() & description(exact:'')";
        "open()" = "stack(mine() | @, 1)";
        "ready()" = "open() ~ stack(wip(), 1)";
        "stack()" = "stack(@)";
        "stack(x)" = "stack(x, 2)";
        "stack(x, n)" = "ancestors(reachable(x, mutable()), n)";
        "symdiff(x, y)" = "(x ~ y) | (y ~ x)";
        "toggle(x)" = "toggle(mega(), x)";
        "toggle(x, y)" = "symdiff(parents(x), y)";
        "wip()" = "null() | description(regex:\"^[A-Z]+:\")";
      };
      aliases = {
        "adopt" =
          let
            jj-adopt = pkgs.writeFishScript "jj-adopt" ''
              argparse -i 'r/revisions=' -- $argv; or exit $status

              set -l rev $_flag_revisions; and if test -z $rev; set rev @; end

              for commit in (${jj} log --no-graph -r "$rev ~ mine()" -T 'commit_id ++ "\n"')
                set -l desc (${jj} log --no-graph -r $commit -T 'description ++ "\nCo-authored-by: " ++ author' | string collect)
                ${jj} metaedit $commit --update-author -m "$desc"
              end
            '';
          in
          [
            "util"
            "exec"
            "--"
            jj-adopt
          ];

        "tug" =
          let
            jj-tug = pkgs.writeFishScript "jj-tug" ''
              argparse -i 'trunk' -- $argv; or exit $status

              set -l from (set -q _flag_trunk; and echo 'trunk()'; or echo 'heads(::@ & bookmarks()) ~ trunk()')

              ${jj} bookmark move -f $from -t 'heads(::@ & mutable() ~ null())' $argv
            '';
          in
          [
            "util"
            "exec"
            "--"
            jj-tug
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
          # Needs two preceding newlines as per GitHub's docs.
          "\nskip-checks"
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

                ${jj} trailer -r $rev Co-authored-by $coauthors
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

  programs.fish =
    let
      subcmds_fn = "_jj_subcmds";
    in
    {
      # Improved subcmd matching behaviour than a plain abbreviation, see:
      #   https://github.com/fish-shell/fish-shell/issues/11944#issuecomment-3420024197
      functions.${subcmds_fn} = ''
        set -l subcmd (string match --groups-only --regex '^jj\s+(\S+)\b' (commandline))
        switch $subcmd
          case ab
            echo abandon
          case anon
            echo log -r "'anon()'"
          case ft
            echo git fetch
          case ps
            echo git push
          case remega rem
            # It's worth running simplify-parents from time to time. See also:
            #   https://github.com/jj-vcs/jj/issues/7711
            #   https://github.com/jj-vcs/jj/issues/6612
            echo rebase -s "'mega()'" -d "'mega()-'" -d "'trunk()'"
          case retrunk ret
            echo rebase -d "'trunk()'"
          case sandwich sw
            echo squash -B "'mega()'" -A "'trunk()'"
          case sq
            echo squash
          case toggle
            echo rebase -s "'mega()'" -d "'toggle()'"
          case '*'
            return 1
        end
      '';

      shellAbbrs = {
        jj_subcmds = {
          command = "jj";
          function = subcmds_fn;
          regex = "\\S+";
        };

        nn = "jj";
        nnui = "jjui";
      };
    };

  programs.jjui = {
    enable = true;
    settings = {
      leader = {
        f = {
          help = "fix";
          send = [
            "$"
            "jj fix"
            "enter"
          ];
        };
        L.help = "revset";
        La = {
          help = "anon()";
          send = [
            "L"
            "anon()"
            "enter"
          ];
        };
        Lh = {
          help = "here()";
          send = [
            "L"
            "here()"
            "enter"
          ];
        };
        r.help = "rebase";
        rt = {
          help = "trunk";
          send = [
            "$"
            "jj rebase -d trunk()"
            "enter"
          ];
        };
        t = {
          help = "trailer";
          context = [ "$change_id" ];
        };
        tc = {
          help = "closes";
          send = [
            "$"
            "jj closes -r $change_id "
          ];
        };
        tf = {
          help = "fixes";
          send = [
            "$"
            "jj fixes -r $change_id "
          ];
        };
        tk = {
          help = "skipchecks";
          send = [
            "$"
            "jj skipchecks -r $change_id"
            "enter"
          ];
        };
        to = {
          help = "coauthor";
          send = [
            "$"
            "jj coauthor -r $change_id"
            "enter"
          ];
        };
        z.help = "resolve";
        zz = {
          help = "interactive";
          send = [
            "$"
            "jj resolve"
            "enter"
          ];
        };
        zm = {
          help = "mergiraf";
          send = [
            "$"
            "jj resolve --tool mergiraf"
            "enter"
          ];
        };
      };
    };
  };

  programs.delta = {
    enable = true;
    enableJujutsuIntegration = true;
  };

  programs.git = {
    enable = true;
    settings = {
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

  home.packages =
    with pkgs;
    let
      but = writeShellScriptBin "but" ''
        exec /Applications/GitButler.app/Contents/MacOS/gitbutler-tauri "$@"
      '';
    in
    [
      but
      git-who
      # For verifying others' commit signatures.
      gnupg
      jjui
      mergiraf
      tig
    ];
}
