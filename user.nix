{ uname, ... }:

{
  users = {
    users.${uname} = {
      # UID needs setting explicitly so that it can be referenced elsewhere.
      uid = 1000;
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      hashedPassword =
        "$6$YP5y35/G6T8zHGaV$8UPmOxWjywESgBX1dEru26EO9G3qBmtEHwqAjt2eaEIr556JyHL8UV.B9VYB9XL1aZGPzR7rVrH5PkV14SYvq1";
    };

    mutableUsers = false;
  };

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/London";

  home-manager.users.${uname}.xdg.userDirs = {
    enable = true;
    documents = "$HOME/docs";
    download = "$HOME/downloads";
    pictures = "$HOME/pics";
  };
}
