# General data backups. Local service backups are handled by the respective
# services.
{ config, pkgs, ... }:

let
  backupsPath = "/home/${config.username}/backups";

  logsBackupsPath = backupsPath + "/logs";
  mailBackupsPath = backupsPath + "/mail";

  radarrLogs = pkgs.writeShellScript "radarr-logs" ''
    endpoint="$(cat ${config.age.secrets.radarr-host.path})/api/v3/movie"
    auth="X-Api-Key: $(cat ${config.age.secrets.radarr-api-key.path})"

    ${pkgs.curl}/bin/curl -s -H "$auth" "$endpoint" | \
        ${pkgs.jq}/bin/jq -r 'map(.title + " HASFILE:" + (.hasFile | tostring)) | join("\n")' > \
          ${logsBackupsPath}/radarr.txt
  '';

  sonarrLogs = pkgs.writeShellScript "sonarr-logs" ''
    endpoint="$(cat ${config.age.secrets.sonarr-host.path})/api/v3/series"
    auth="X-Api-Key: $(cat ${config.age.secrets.sonarr-api-key.path})"

    ${pkgs.curl}/bin/curl -s -H "$auth" "$endpoint" | \
        ${pkgs.jq}/bin/jq 'map(.title) | join(", ")' > \
          ${logsBackupsPath}/sonarr.txt
  '';
in
{

  age.secrets = {
    migadu = {
      file = ../secrets/migadu.age;
      # For offlineimap.
      owner = config.username;
    };

    radarr-api-key.file = ../secrets/radarr-api-key.age;
    radarr-host.file = ../secrets/radarr-host.age;
    sonarr-api-key.file = ../secrets/sonarr-api-key.age;
    sonarr-host.file = ../secrets/sonarr-host.age;
  };

  # New buckets must be created manually and have their file lifecycle set as
  # appropriate.
  services.restic.backups =
    let
      baseCfg = {
        passwordFile = config.age.secrets.restic.path;
        environmentFile = config.age.secrets.b2-env.path;
        pruneOpts = [
          "--keep-daily 7"
          "--keep-weekly 4"
          "--keep-monthly 6"
        ];
        timerConfig.OnCalendar = "04:00";
        extraBackupArgs =
          let
            xs = [
              "@eaDir"
              ".DS_Store"
            ];
          in
          map (x: "--exclude=${x}") xs;
      };
    in
    {
      snorlax = baseCfg // {
        repository = "b2:snorlax-restic2";
        paths = [ backupsPath ];
      };
    };

  services.offlineimap = {
    enable = true;
    install = true;
  };

  home-manager.users.${config.username} = {
    programs.offlineimap.enable = true;

    accounts.email.accounts.main =
      let
        host = "imap.migadu.com";
      in
      {
        primary = true;
        imap.host = host;
        offlineimap = {
          enable = true;
          extraConfig = {
            local.localfolders = mailBackupsPath;
            remote = {
              type = "IMAP";
              remotehost = host;
              remoteuser = config.email.address;
              remotepassfile = config.age.secrets.migadu.path;
              folderfilter = "lambda folder: folder in ['Archive', 'Awaiting', 'Unfulfilled', 'INBOX', 'Sent']";
            };
          };
        };
      };
  };

  systemd = {
    services = {
      "radarr-logs" = {
        description = "Radarr logs";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = radarrLogs;
        };
      };

      "sonarr-logs" = {
        description = "Sonarr logs";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = sonarrLogs;
        };
      };
    };

    timers = {
      "radarr-logs" = {
        description = "Run Radarr logs";
        wantedBy = [ "timers.target" ];
        timerConfig.OnCalendar = "daily";
      };

      "sonarr-logs" = {
        description = "Run Sonarr logs";
        wantedBy = [ "timers.target" ];
        timerConfig.OnCalendar = "daily";
      };
    };
  };
}
