{ config, ... }:

{
  users = {
    users.${config.username} = {
      # UID needs setting explicitly so that it can be referenced elsewhere.
      uid = 1000;
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      hashedPassword =
        "$6$YP5y35/G6T8zHGaV$8UPmOxWjywESgBX1dEru26EO9G3qBmtEHwqAjt2eaEIr556JyHL8UV.B9VYB9XL1aZGPzR7rVrH5PkV14SYvq1";
    };

    mutableUsers = false;
  };

  # This, if set, allows the specified user to be automatically logged in
  # without a password.
  #
  # Whilst this'd be reasonably secure on TTY1 directly following LUKS, it
  # becomes a vulnerability when one can freely switch to another TTY. Most
  # lockers don't prevent this.
  services.getty.autologinUser = null;
}
