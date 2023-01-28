{ ... }:

{
  age.secrets = {
    ddns-token.file = ../../secrets/ddns-token.age;
    pihole-env.file = ../../secrets/pihole-env.age;
    radarr-api-key.file = ../../secrets/radarr-api-key.age;
    radarr-host.file = ../../secrets/radarr-host.age;
    radicale-htpasswd = {
      file = ../../secrets/radicale-htpasswd.age;
      owner = "radicale";
    };
    sonarr-api-key.file = ../../secrets/sonarr-api-key.age;
    sonarr-host.file = ../../secrets/sonarr-host.age;
    zwave-env.file = ../../secrets/zwave-env.age;
  };
}
