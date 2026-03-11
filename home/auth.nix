{ ... }:

{
  programs.ssh = {
    enable = true;
    extraConfig = ''
      IdentityAgent ~/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
    '';
  };

  programs.fish.shellInit = ''
    set -x SSH_AUTH_SOCK /Users/sam/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
  '';
}
