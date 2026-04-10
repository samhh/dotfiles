{
  fish,
  lib,
  writeTextFile,
  ...
}:

{
  writeFishScript =
    name: text:
    writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!${lib.getExe fish}
        ${text}
      '';
      checkPhase = ''
        ${lib.getExe fish} --no-execute "$target"
      '';
    };
}
