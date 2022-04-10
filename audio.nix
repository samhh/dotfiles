{ pkgs, nasPath, uname, ... }:

let musicDir = nasPath + "/music/archive";
in {
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
  };

  home-manager.users.${uname} = {
    services.mpd = {
      enable = true;
      musicDirectory = musicDir;
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "foobar"
        }
      '';
    };

    services.mpdris2.enable = true;

    programs.beets = {
      enable = true;
      settings = {
        directory = musicDir;
        library = musicDir + "/beets.blb";
        paths = {
          default = "Albums/$albumartist/($year) $album%aunique{}/$track - $title";
          singleton = "Singles/$artist/$title";
          comp = "Compilations/$album%aunique{}/$track - $title";
	};
        plugins = [ "replaygain" "fetchart" "smartplaylist" "mpdupdate" ];
        replaygain = {
          backend = "gstreamer";
	  overwrite = true;
	};
      };
    };

    home.packages = with pkgs; [
      pavucontrol
    ];
  };
}
