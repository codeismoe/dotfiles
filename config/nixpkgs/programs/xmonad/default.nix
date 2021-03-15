{ pkgs, ... }:

let
  extra = ''
    ${pkgs.nitrogen}/bin/nitrogen --restore &
  '';
in
{
  xsession = {
    enable = true;
    initExtra = extra;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
      ];
      config = ./config.hs;
    };
  };
}
