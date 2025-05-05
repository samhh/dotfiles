{
  fish,
  lib,
  writeScript,
  ...
}:

{
  writeFishScript =
    name: text:
    writeScript name ''
      #!${lib.getExe fish}
      ${text}
    '';
}
