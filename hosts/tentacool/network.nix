{ hostName, ... }:

{
  networking = {
    inherit hostName;
  };
}
