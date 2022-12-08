{ config, ... }:

{
  services.offlineimap = {
    enable = true;
    install = true;
  };

  home-manager.users.${config.username} = {
    programs.offlineimap.enable = true;

    accounts.email.accounts.main =
      let host = "imap.migadu.com";
      in
      {
        primary = true;
        imap.host = host;
        offlineimap = {
          enable = true;
          extraConfig = {
            local.localfolders = config.nas.path + "/mail/";
            remote = {
              type = "IMAP";
              remotehost = host;
              remoteuser = config.email.address;
              remotepassfile = config.age.secrets.migadu.path;
              folderfilter =
                "lambda folder: folder in ['Archive', 'Awaiting', 'Unfulfilled', 'Jobs 2021', 'INBOX', 'Sent']";
            };
          };
        };
      };
  };
}
