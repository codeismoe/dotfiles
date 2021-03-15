{ config, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet
    nitrogen
  ];
in
{

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;

  imports = (import ./programs) ++ (import ./services); 
  xdg.enable = true;
  news.display = "silent";
  
  programs = {
    direnv = {
      enable = true;
      enableFishIntegration = true;
      enableNixDirenvIntegration = true;
    };
    firefox.enable = true;
  };

  home.packages = xmonadPkgs;
  home.username = "pks";
  home.homeDirectory = "/home/pks";
  home.sessionVariables = { 
    EDITOR="vim"; 
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
