{ config, pkgs, ... }:

let
  restic-wrapper = pkgs.writeShellScriptBin "restic-wrapper" ''
    set -a
    . ${config.age.secrets.b2-env.path}
    set +a

    exec ${pkgs.restic}/bin/restic -p ${config.age.secrets.restic.path} "$@"
  '';
in
{
  age = {
    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    secrets = {
      b2-env = {
        file = ../secrets/b2-env.age;
        # For restic-wrapper.
        owner = config.username;
      };
      migadu = {
        file = ../secrets/migadu.age;
        # For aerc & offlineimap.
        owner = config.username;
        group = "users";
      };
      restic = {
        file = ../secrets/restic.age;
        # For restic-wrapper.
        owner = config.username;
      };
    };
  };

  environment.systemPackages = with pkgs; [ restic restic-wrapper ];
}
