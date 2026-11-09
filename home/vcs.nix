{
  config,
  pkgs,
  ...
}:

let
  name = "Sam A. Horvath-Hunt";
  email = "hello@samhh.com";
  signing-key = "~/.ssh/id_ed25519";
in
{
  programs.jujutsu = {
    enable = false;
    package = null;
    settings = {
      user = {
        inherit name email;
      };
      signing = {
        behavior = "own";
        backend = "ssh";
        key = signing-key;
      };
    };
  };

  # Workaround for programs.jujutsu.package being broken when null.
  xdg.configFile."jj/config.toml".source =
    (pkgs.formats.toml { }).generate "jujutsu-config"
      config.programs.jujutsu.settings;

  programs.git = {
    enable = true;
    package = null;
    signing = {
      format = "ssh";
      key = signing-key;
    };
    settings = {
      user = {
        inherit name email;
      };
      commit.gpgSign = true;
    };

    ignores = [
      ".DS_Store"
      ".codex/"
      ".helix/"
      ".zed/"
    ];
  };
}
