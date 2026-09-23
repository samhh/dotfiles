{
  config,
  pkgs,
  ...
}:

let
  name = "Sam A. Horvath-Hunt";
  email = "hello@samhh.com";
  pub-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHrtxBybcDSLc41RPT3tmmLBXRlaqpPfUOtYF86aWlIA";
  allowed-signers = builtins.toFile "allowed-signers" ''
    ${email} ${pub-key}
  '';

  jj-trailer = pkgs.writeFishScript "jj-trailer" ''
    argparse -N 2 'r/revisions=' -- $argv; or exit $status

    set -l rev $_flag_revisions
    set -l key $argv[1]
    set -l vals $argv[2..]

    set -l trailers

    for i in (seq (count $vals))
      set trailers[$i] "$key: $vals[$i]"
    end

    for commit in (jj log --no-graph -r $rev -T 'commit_id ++ "\n"')
      set -l prev (jj log --no-graph -r $commit -T description | string collect)
      jj desc $commit -m "$prev" -m "$(string join \n $trailers)"
    end
  '';
in
{
  programs.jujutsu = {
    enable = false;
    package = null;
    settings = {
      user = {
        inherit name email;
      };
      signing = {
        behavior = "drop";
        backend = "ssh";
        backends.ssh = {
          inherit allowed-signers;
        };
        key = pub-key;
      };
      git.sign-on-push = true;
      templates.git_push_bookmark = "\"samhh/\" ++ change_id.shortest(3)";
      ui.default-command = [
        "log"
        "-r"
        "here()"
      ];
      template-aliases = {
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      # The stack() idea comes from:
      #   https://gist.github.com/thoughtpolice/8f2fd36ae17cd11b8e7bd93a70e31ad6
      #   https://andre.arko.net/2025/09/28/stupid-jj-tricks/#revsets
      revset-aliases = {
        "anon()" = "stack(mine() ~ ::remote_bookmarks(), 1)";
        "here()" = "(trunk()..@)::";
        # trunk() points to remote which isn't always what we want when local diverges. See also:
        #   https://github.com/jj-vcs/jj/issues/7990
        "local_trunk()" = "bookmarks(glob:'{trunk,master,main}')";
        "null()" = "empty() & description(exact:'')";
        "stack()" = "stack(@)";
        "stack(x)" = "stack(x, 2)";
        "stack(x, n)" = "ancestors(reachable(x, mutable()), n)";
      };
      aliases = {
        ab = [ "abandon" ];
        anon = [
          "log"
          "-r"
          "anon()"
        ];
        ft = [
          "git"
          "fetch"
        ];
        ps = [
          "git"
          "push"
        ];
        retrunk = [
          "rebase"
          "-d"
          "trunk()"
        ];
        sq = [ "squash" ];

        "tug" =
          let
            jj-tug = pkgs.writeFishScript "jj-tug" ''
              argparse -i 't/trunk' -- $argv; or exit $status

              if set -q _flag_trunk
                jj bookmark move -f 'local_trunk()' -t 'heads(::@ & mutable() ~ null())' $argv
              else
                jj bookmark move -f 'heads(::@ & bookmarks()) ~ trunk()' -t 'heads(::@ & mutable() ~ null())' $argv
              end
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
        #   - GitHub: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword
        "closes" = [
          "trailer"
          "Closes"
        ];

        # Supported by:
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
      };
    };
  };

  # Workaround for programs.jujutsu.package being broken when null.
  xdg.configFile."jj/config.toml".source =
    (pkgs.formats.toml { }).generate "jujutsu-config"
      config.programs.jujutsu.settings;

  programs.delta = {
    enable = true;
    enableJujutsuIntegration = true;
  };

  programs.git = {
    enable = true;
    package = null;
    signing = {
      format = "ssh";
      key = pub-key;
    };
    settings = {
      user = {
        inherit name email;
      };
      gpg = {
        ssh.allowedSignersFile = allowed-signers;
      };
      commit.gpgSign = true;
      tag.gpgSign = true;
      url."git@github.com:".insteadOf = "gh:";
      init.defaultBranch = "trunk";
    };

    ignores = [ ".DS_Store" ];
  };
}
