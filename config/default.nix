# Custom configuration accessible on `config`. Avoids the need to pass around
# `specialArgs`.

{ lib, ... }:

with lib; {
  options = {
    username = mkOption { type = types.str; };
    fullName = mkOption { type = types.str; };
    email = {
      address = mkOption { type = types.str; };
    };
  };
}
