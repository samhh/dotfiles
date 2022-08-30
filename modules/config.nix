{ lib, ... }:

with lib; {
  # Custom configuration accessible on `config`. Avoids the need to pass around
  # `specialArgs`.
  options = {
    username = mkOption { type = types.str; };
    fullName = mkOption { type = types.str; };
    email = {
      address = mkOption { type = types.str; };
      pass.path = mkOption { type = types.str; };
    };
    nas.path = mkOption { type = types.str; };
  };
}
