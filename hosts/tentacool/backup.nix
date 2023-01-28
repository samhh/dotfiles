# General data backups. Local service backups are handled by the respective
# services.
{ config, pkgs, ... }:

let
  sonarrLogs = pkgs.writeShellScript "sonarr-logs" ''
    endpoint="$(cat ${config.age.secrets.sonarr-host.path})/api/v3/series"
    auth="X-Api-Key: $(cat ${config.age.secrets.sonarr-api-key.path})"

    ${pkgs.curl}/bin/curl -s -H "$auth" "$endpoint" | \
        ${pkgs.jq}/bin/jq 'map(.title) | join(", ")' > \
          ${config.nas.path}/logs/sonarr.txt
  '';
in
{
  # The B2 buckets presumably need creating in the web UI first. Remember to
  # set file lifecycle to "keep only the last version".
  services.restic.backups =
    let
      baseCfg =
        {
          initialize = true;
          passwordFile = config.age.secrets.restic.path;
          environmentFile = config.age.secrets.b2-env.path;
          pruneOpts = [
            "--keep-daily 7"
            "--keep-weekly 4"
            "--keep-monthly 6"
          ];
          timerConfig.OnCalendar = "04:00";
          extraBackupArgs = map (x: "--exclude=${x}") config.nas.hiddenFiles;
        };
      # The bucket names appended with "2" aren't typos. There was a bug in the
      # UI when I tried to create them matching the usual schema.
    in
    {
      archive = baseCfg // {
        repository = "b2:archive-restic2";
        paths = [ (config.nas.path + "/archive/") ];
      };

      keys = baseCfg // {
        repository = "b2:keys-restic";
        paths = [ (config.nas.path + "/keys/") ];
      };

      logs = baseCfg // {
        repository = "b2:logs-restic";
        paths = [ (config.nas.path + "/logs/") ];
      };

      mail = baseCfg // {
        repository = "b2:mail-restic2";
        paths = [ (config.nas.path + "/mail/") ];
      };

      manuals = baseCfg // {
        repository = "b2:manuals-restic";
        paths = [ (config.nas.path + "/manuals/") ];
      };
    };

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

  systemd = {
    services."sonarr-logs" = {
      description = "Sonarr logs";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = sonarrLogs;
      };
    };

    timers."sonarr-logs" = {
      description = "Run Sonarr logs";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
