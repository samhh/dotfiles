{ pkgs, ... }:

let
  uname = "sam";
in {
  home-manager.users.${uname} = {
    programs.offlineimap.enable = true;

    accounts.email.accounts.main = {
      primary = true;
      imap.host = "imap.migadu.com";
      offlineimap = {
        enable = true;
        extraConfig = {
          local.localfolders = "/mnt/nas/mail/";
          remote = {
            type = "IMAP";
            remotehost = "imap.migadu.com";
            remoteuser = "hello@samhh.com";
            remotepasseval = "get_pass(\"emails/migadu.com/mailbox/hello\")";
            # sslcacertfile = "/etc/ssl/certs/ca-certificates.crt";
            folderfilter = "lambda folder: folder in ['Archive', 'Unfulfilled Orders', 'INBOX', 'Sent']";
          };
        };
      };
    };

    xdg.configFile."vdirsyncer/config".source = ./cfg/vdirsyncer;

    home.packages = with pkgs; [
      backblaze-b2
      duplicity
      vdirsyncer
    ];
  };
}
