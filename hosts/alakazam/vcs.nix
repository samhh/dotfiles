{ config, pkgs, email, emailPassPath, uname, ... }:

let
  editorBin = "${config.home-manager.users.${uname}.programs.neovim.finalPackage}/bin/nvim";
in {
  home-manager.users.${uname}.programs.git = {
    enable = true;
    # For send-email support.
    package = pkgs.gitFull;
    delta.enable = true;
    extraConfig = {
      push.default = "simple";
      pull.ff = "only";
      user = {
        name = "Sam A. Horvath-Hunt";
        email = email;
        signingkey = "4667250BD56735A8";
      };
      credential."smtp://hello%40samhh.com@smtp.migadu.com%3a465".helper =
        "!${./scripts/pass-git-credential.sh} ${emailPassPath}";
      sendemail = {
        smtpserver = "smtp.migadu.com";
        smtpuser = email;
        smtpencryption = "ssl";
      };
      url = {
        "git@git.sr.ht:~".insteadOf = "sh:";
        "git@github.com:".insteadOf = "gh:";
        "ssh://aur@aur.archlinux.org/".insteadOf = "aur:";
      };
      init.defaultBranch = "master";
      commit.gpgSign = true;
      tag.gpgSign = true;
      merge.tool = "vimdiff";
      mergetool = {
        vimdiff.path = editorBin;
        keepBackup = false;
      };
      blame.date = "short";
    };
    aliases = {
      br = "branch";
      bs = "bisect";
      co = "checkout";
      df = "diff";
      dfs = "diff --staged";
      ca = "commit --amend --no-edit";
      cam = "commit --amend";
      cm = "commit -m";
      cma = "commit -am";
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
      sw-gh-pr = "!sh -c 'git ft origin pull/$0/head:pr/$0 && git sw pr/$0'";
      wt = "worktree";
    };
  };
}
