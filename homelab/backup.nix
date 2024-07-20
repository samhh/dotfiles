# General data backups. Local service backups are handled by the respective
# services.
{ config, pkgs, ... }:

let
  backupsPath = "/home/${config.username}/backups";

  logsBackupsPath = backupsPath + "/logs";

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
    radarr-api-key.file = ../secrets/radarr-api-key.age;
    radarr-host.file = ../secrets/radarr-host.age;
    sonarr-api-key.file = ../secrets/sonarr-api-key.age;
    sonarr-host.file = ../secrets/sonarr-host.age;
  };

  services.restic.backups.snorlax = {
    repository = "b2:snorlax-restic2";
    paths = [ backupsPath ];
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
