{ inputs }:
{ config, pkgs, lib, ... }:

{
  imports = [
    ./niri.nix
    ./waybar.nix
    ./emacs/default.nix
  ];

  home = {
    stateVersion = "23.11";
    username = "catbrick";
    homeDirectory = "/home/catbrick";
    sessionVariables.NIXOS_OZONE_WL = "1";


    packages =  with pkgs; [
      vlc
      vscode
      blender
      
      any-nix-shell
      mpc
      ncmpcpp
      cantata
      signal-desktop
      # is it secure? no. Do I care? No.
      protonvpn-gui
      devcontainer
      nautilus
      swww

      # cli
      (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
      wl-clipboard
      discord
      
      curl
      file
      htop
      ispell
      jq
      neofetch
      pass
      ripgrep
      texliveFull
      unzip


      # aesthetics
      catppuccin
      catppuccin-cursors
      catppuccin-gtk
      catppuccin-qt5ct
      roboto
      tela-icon-theme

      # applications
      deluge
      firefox
      gimp
      google-chrome
      mpv
      pavucontrol
      qemu
      wdisplays
      nicotine-plus
      brightnessctl
      ghostscript
      gnupg

      # gaymes
      nethack

      xwayland-satellite-unstable

      # development
      gcc
      gnumake
    ];
  };

  programs.home-manager.enable = true;

  programs.tmux = {
    enable = true;
    escapeTime = 0;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.git = {
    enable = true;
    settings.user.name = "Cat Brick";
    settings.user.email = "me@lily.bike";
    ignores = [ ".DS_Store" "*~" "*.swp" ".vim" "#*"];
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
        any-nix-shell fish --info-right | source
        starship init fish | source
      '';
    plugins = [];
  };
  programs.starship.enable = true;

  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
  };

  stylix.targets.emacs.enable = false;
  services.emacs.enable = true;

  programs.fuzzel.enable = true;
  programs.swaylock.enable = true;

  services.swww.enable = true;
  services.lorri.enable = true;
  services.mako.enable = true;
  services.udiskie.enable = true;
  services.gpg-agent.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "~/Music/";
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "Pipewire"
      }
    '';
  };

  services.swayidle.enable = true; # idle management daemon
  services.polkit-gnome.enable = true; # polkit
  
  fonts.fontconfig.enable = true;
}
