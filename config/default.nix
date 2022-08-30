{ lib, ... }:

with lib; {
  # Custom configuration accessible on `config`. Avoids the need to pass around
  # `specialArgs`.
  options = {
    # System-agnostic.
    username = mkOption { type = types.str; };
    fullName = mkOption { type = types.str; };
    email = {
      address = mkOption { type = types.str; };
      pass.path = mkOption { type = types.str; };
    };
    nas.path = mkOption { type = types.str; };

    # Potentially system-specific.
    apps = {
      terminal.bin = mkOption { type = types.str; };
      launcher.bin = mkOption { type = types.str; };
      webBrowser.bin = mkOption { type = types.str; };
      streamer.bin = mkOption { type = types.str; };
    };
  };
}
