{ pkgs, ... }:

{
  age = {
    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    # Whilst the timed backups only run on Tentacool, Alakazam should have
    # access at any time for restores or whatever else. At present Alakazam
    # will need to enhance the environment with the B2 account environment
    # variables manually, for example:
    #
    # $ B2_ACCOUNT_ID=foo B2_ACCOUNT_KEY=bar restic snapshots -r b2:bucket-name
    secrets = {
      b2-env.file = ../secrets/b2-env.age;
      restic.file = ../secrets/restic.age;
    };
  };

  environment.systemPackages = with pkgs; [ agenix restic ];
}
