{ pkgs, ... }:

{
  services.greetd = {
    enable = true;

    settings.default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --asterisks --cmd sway";
  };

  # Prevent boot logs from being printed atop the greeter by moving us to TTY2:
  #   https://github.com/apognu/tuigreet/issues/17#issuecomment-949757598
  boot.kernelParams = [ "console=tty1" ];
  services.greetd.vt = 2;
}
