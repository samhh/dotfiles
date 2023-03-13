{ config, ... }:

{
  age.secrets = {
    gmail = {
      file = ../../secrets/gmail.age;
      # For aerc & offlineimap.
      owner = config.username;
      group = "users";
    };

    krabby = {
      file = ../../secrets/krabby.age;
      # For vdirsyncer.
      owner = config.username;
      group = "users";
    };
  };
}
