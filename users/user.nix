{ config, pkgs, lib, ... }:

{
  imports = [
    ./niri.nix
    ./waybar.nix
    ./emacs/default.nix
  ];

  home = {
    stateVersion = "25.11";
    username = "catbrick";
    homeDirectory = "/home/catbrick";
    sessionVariables.NIXOS_OZONE_WL = "1";


    packages =  with pkgs; [
      vlc
      blender
      gnuplot
      zotero
      vintagestory
      discord

      any-nix-shell
      cantata
      signal-desktop

      protonvpn-gui
      nautilus

      hunspell
      aspell
      aspellDicts.en
      hunspellDicts.en_US-large
      wl-clipboard

      curl
      file
      htop
      ispell
      jq
      fastfetch
      pass
      ripgrep
      unzip

      pandoc
      graphviz

      # aesthetics
      catppuccin
      catppuccin-cursors
      catppuccin-gtk
      catppuccin-qt5ct
      roboto

      # applications
      deluge
      firefox
      qbz
      gimp
      google-chrome
      mpv
      pavucontrol
      qemu
      wdisplays
      brightnessctl
      ghostscript
      gnupg
    ];
  };

  programs.home-manager.enable = true;

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    historyLimit = 100000;
    mouse = true;
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

  # stylix.targets.emacs.enable = false;

  services.emacs.enable = true;

  programs.fuzzel.enable = true;
  programs.swaylock.enable = true;

  services.lorri.enable = true;
  services.mako.enable = true;
  services.udiskie.enable = true;
  services.gpg-agent.enable = true;

  # services.mpd = {
  #   enable = true;
  #   musicDirectory = "~/Music/";
  #   extraConfig = ''
  #     audio_output {
  #       type "pipewire"
  #       name "Pipewire"
  #     }
  #   '';
  # };

  services.polkit-gnome.enable = true;
  fonts.fontconfig.enable = true;
}
