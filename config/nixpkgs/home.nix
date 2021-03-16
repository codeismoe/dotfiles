{ config, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet
    nitrogen
  ];
  otherPkgs = with pkgs; [
    spotify
    slack
    discord
    
    pavucontrol
    qutebrowser
    tmux

    gimp
  ];
in
{

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;

  imports = (import ./programs); 
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

  home.packages = xmonadPkgs ++ otherPkgs;
  home.username = "pks";
  home.homeDirectory = "/home/pks";
  home.sessionVariables = { 
    EDITOR="vim"; 
  };

  services.lorri.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      name = "Qogir";
      package = pkgs.qogir-icon-theme;
    };
    theme = {
      name = "Matcha";
      package = pkgs.matcha-gtk-theme;
    };
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
