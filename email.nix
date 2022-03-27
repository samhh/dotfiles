{ pkgs, ... }:

let
  uname = "sam";

  # Patched not to check the permissions of `accounts.conf` which is managed by
  # Home Manager below.
  aerc = pkgs.aerc.overrideAttrs (attrs: {
    patches = attrs.patches ++ [ ./patches/aerc-no-perms-check.patch ];
  });
in {
  home-manager.users.${uname} = {
    xdg.configFile.aerc.source = ./cfg/aerc;

    home.packages = with pkgs; [
      aerc
      urlscan
    ];
  };
}
