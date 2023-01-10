{ config, ... }:

let
  stick = "/dev/serial/by-id/usb-Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_c6970d0f91bcea11a1ec96e368aed703-if00-port0";
  webPort = 8091;
in
{
  networking.firewall.allowedTCPPorts = [
    webPort
  ];

  virtualisation.oci-containers.containers.zwavejs2mqtt = {
    image = "zwavejs/zwave-js-ui:8.6.2";
    volumes = [
      "zwavejs2mqtt:/usr/src/app/store"
    ];
    extraOptions = [
      "--device=${stick}:/dev/zwave"

      # Container needs access to Spearow on host.
      "--network=host"
    ];
    environment = {
      TZ = "Europe/London";
      PORT = toString webPort;
    };
    environmentFiles = [
      config.age.secrets.zwave-env.path
    ];
  };
}
