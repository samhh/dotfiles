{ ... }:

{
  age.secrets = {
    ddns-token.file = ../../secrets/ddns-token.age;
    pihole-env.file = ../../secrets/pihole-env.age;
    radicale-htpasswd = {
      file = ../../secrets/radicale-htpasswd.age;
      owner = "radicale";
    };
    zwave-env.file = ../../secrets/zwave-env.age;
  };
}
