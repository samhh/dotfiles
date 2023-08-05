{ ... }:

{
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };
}
