{ config, pkgs, ... }:

let
  editorBin = "${config.home-manager.users.${config.username}.programs.neovim.finalPackage}/bin/nvim";
in
{
  home-manager.users.${config.username}.programs.git = {
    enable = true;

    delta.enable = true;

    extraConfig = {
      push.default = "simple";
      pull.ff = "only";
      user = {
        name = config.fullName;
        email = config.email.address;
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
        vimdiff.path = editorBin;
        keepBackup = false;
      };
      blame.date = "short";
    };

    aliases = {
      br = "branch";
      brd = "branch -D @{-1}";
      df = "diff";
      dfs = "diff --staged";
      ca = "commit --amend --no-edit";
      cam = "commit --amend";
      cm = "commit";
      cma = "commit -a";
      cmx = "commit --fixup";
      cp = "cherry-pick";
      ft = "fetch";
      ignore = "!sh -c 'git add -N $0 && git update-index --skip-worktree $0'";
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
      sw-gh-pr = "!sh -c 'git ft origin pull/$0/head:pr/$0 && git sw pr/$0'";
      wt = "worktree";
    };

    # send-email
    package = pkgs.gitFull;
    extraConfig = {
      sendemail = {
        smtpserver = "smtp.migadu.com";
        smtpuser = config.email.address;
        smtpencryption = "ssl";
      };
      credential."smtp://hello%40samhh.com@smtp.migadu.com%3a465".helper =
        let
          script = pkgs.writeShellScriptBin "cat-git-credential" ''
            echo "password=$(cat "$1")"
          '';
        in
        "!${script}/bin/cat-git-credential ${config.age.secrets.migadu.path}";
    };
  };
}
