{ ... }:

{
  programs.fish = {
    enable = true;

    shellInit = ''
      source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
      brew shellenv fish | source
      if not set -q VISUAL; set -x VISUAL hx; end
    '';

    interactiveShellInit = ''
      set -g fish_greeting
      starship init fish | source
      zoxide init fish | source
    '';
  };

  home.file.".hushlogin".text = "";

  home.sessionPath = [
    # Rustup
    "/opt/homebrew/opt/rustup/bin"
    # GitButler
    "$HOME/.local/bin"
  ];
}
