{ ... }:

let
  uname = "sam";
in {
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      users = [ uname ];
      keepEnv = true;
    }];
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "qt";
  };

  programs.ssh.startAgent = true;
}
