{ config, ... }:

{
  age.secrets = {
    krabby = {
      file = ../../secrets/krabby.age;
      # For vdirsyncer.
      owner = config.username;
      group = "users";
    };
  };
}
