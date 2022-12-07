{ config, pkgs, ... }:

{
  home-manager.users.${config.username} = {
    programs.offlineimap = {
      enable = true;
      pythonFile = ''
        from subprocess import check_output
        def get_pass(path):
            return check_output("pass show " + path, shell=True).splitlines()[0]
      '';
    };

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
              remotepasseval = "get_pass(\"${config.email.pass.path}\")";
              folderfilter =
                "lambda folder: folder in ['Archive', 'Awaiting', 'Unfulfilled', 'Jobs 2021', 'INBOX', 'Sent']";
            };
          };
        };
      };

    xdg.configFile."vdirsyncer/config".source = ./cfg/vdirsyncer;

    home.packages = with pkgs; [
      vdirsyncer
    ];
  };
}
