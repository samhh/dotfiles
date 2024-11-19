{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:

let
  pub-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICF3PGCLbd7QTcz4cSYONosH7tyJFsncXDTA/qRBo7/A";
  jj-attr =
    let
      fzf = "${pkgs.fzf}/bin/fzf";
      git = "${pkgs.git}/bin/git";
      jj = "${pkgs-unstable.jujutsu}/bin/jj";
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
      jj = "${pkgs-unstable.jujutsu}/bin/jj";
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
        name = "Sam A. Horvath-Hunt";
        email = "hello@samhh.com";
      };
      signing = {
        sign-all = true;
        backend = "ssh";
        key = pub-key;
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
          "(trunk()..@)::"
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
      # Until there's a programs.jujutsu.delta.enable option:
      #   https://github.com/nix-community/home-manager/issues/4887
      ui = {
        pager = "${pkgs.delta}/bin/delta";
        diff.format = "git";
      };
    };
  };

  # Will probably be fixed in 24.11, see:
  #   https://github.com/nix-community/home-manager/pull/5207
  #   https://github.com/nix-community/home-manager/pull/5416
  home.sessionVariables = lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin) {
    JJ_CONFIG = "${config.xdg.configHome}/jj/config.toml";
  };

  programs.git = {
    enable = true;

    extraConfig = {
      user = {
        name = "Sam A. Horvath-Hunt";
        email = "hello@samhh.com";
        signingkey = pub-key;
      };
      gpg.format = "ssh";
      tag.gpgSign = true;
      url = {
        "git@git.sr.ht:~".insteadOf = "sh:";
        "git@github.com:".insteadOf = "gh:";
      };
      init.defaultBranch = "master";
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
