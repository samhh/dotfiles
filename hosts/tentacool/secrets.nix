{ ... }:

{
  age.secrets = {
    ddns-token.file = ../../secrets/ddns-token.age;
    pihole-pass.file = ../../secrets/pihole-pass.age;
    radicale-htpasswd = {
      file = ../../secrets/radicale-htpasswd.age;
      owner = "radicale";
    };
  };
}
