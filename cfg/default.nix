# Custom configuration accessible on `config`. Avoids the need to pass around
# `specialArgs`.

{ lib, ... }:

with lib; {
  options = {
    # System-agnostic.
    username = mkOption { type = types.str; };
    fullName = mkOption { type = types.str; };
    email = {
      address = mkOption { type = types.str; };
      pass.path = mkOption { type = types.str; };
    };
    nas = {
      path = mkOption { type = types.str; };
      hiddenFiles = mkOption {
        type = types.listOf types.str;
        description = "Files which you'd typically want excluded, such as metadata and other dotfiles.";
      };
    };

    # Potentially system-specific.
    apps = {
      terminal.bin = mkOption { type = types.str; };
      launcher.bin = mkOption { type = types.str; };
      webBrowser.bin = mkOption { type = types.str; };
    };
  };

  config = {
    username = "sam";
    fullName = "Sam A. Horvath-Hunt";
    email = {
      address = "hello@samhh.com";
      pass.path = "emails/migadu.com/mailbox/hello";
    };
    nas = {
      path = "/mnt/nas";
      hiddenFiles = [ "@eaDir" ".DS_Store" ];
    };
  };
}
