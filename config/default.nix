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
    nas = {
      path = mkOption { type = types.str; };
      hiddenFiles = mkOption {
        type = types.listOf types.str;
        description = "Files which you'd typically want excluded, such as metadata and other dotfiles.";
      };
    };
  };
}
