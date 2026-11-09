{
  config,
  pkgs,
  ...
}:

let
  name = "Sam A. Horvath-Hunt";
  email = "hello@samhh.com";
  pub-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHrtxBybcDSLc41RPT3tmmLBXRlaqpPfUOtYF86aWlIA";
  allowed-signers = builtins.toFile "allowed-signers" ''
    ${email} ${pub-key}
  '';
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
        behavior = "drop";
        backend = "ssh";
        backends.ssh = {
          inherit allowed-signers;
        };
        key = pub-key;
      };
      git.sign-on-push = true;
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
      key = pub-key;
    };
    settings = {
      user = {
        inherit name email;
      };
      gpg = {
        ssh.allowedSignersFile = allowed-signers;
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
