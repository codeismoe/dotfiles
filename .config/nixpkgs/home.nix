{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "patch";
  home.homeDirectory = "/home/patch";

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    discord
    k3s
    kubernetes-helm
    runelite
    kompose
    curl
    firefox
    mpv
    kitty
    any-nix-shell
    emacs
    fira-code
    iosevka
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
    python39
    python-language-server
    ispell
    filezilla
    steam
    krita
    spotify
    gimp
    blender
    texlive.combined.scheme-basic
    wget
    unzip
    ripgrep
    autoconf
    automake
    ledger
  ];

  programs.tmux = {
	  enable = true;
	  escapeTime = 0;
  };

  programs.direnv = {
    enable = true;
  };

  programs.git = {
	  enable = true;
	  userName = "Cat Brick";
	  userEmail = "me@lily.bike";
	  ignores = [ ".DS_Store" "*~" "*.swp" ".vim"];
  };

  programs.fish = {
    enable = true;
    promptInit = ''
        any-nix-shell fish --info-right | source
        starship init fish | source
      '';

    plugins = [];
  };

  programs.starship = {
    enable = true;
  };

  services.lorri.enable = true;

  home.stateVersion = "21.11";
}
