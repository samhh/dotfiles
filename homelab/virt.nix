{ ... }:

{
  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [ "--all" ];
      dates = "3 months";
    };
  };
}
