{ uname, ... }:

{
  services.ddclient = {
    enable = true;
    protocol = "namecheap";
    username = "samhh.com";
    passwordFile = "/home/${uname}/.ddnspw";
  };
}
