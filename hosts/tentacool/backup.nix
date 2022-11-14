{ config, ... }:

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
          extraBackupArgs = [
            "--exclude=@eaDir/"
            "--exclude=.DS_Store"
          ];
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
}
