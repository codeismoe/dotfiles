{ config, pkgs, ... }:

{

  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "patchwork";
  home.homeDirectory = "/home/patchwork";

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    discord
    nodejs
    signal-desktop
    curl
    firefox
    kitty
    any-nix-shell
    emacs
    iosevka
    (nerdfonts.override { fonts = [ "Iosevka" "Inconsolata" ]; })
    ispell
    spotify
    wget
    unzip
    ripgrep
    automake
    maim
    rustup
    clang
    brightnessctl
    ghc
    cabal-install
    haskell-language-server
    clang-tools
    gnumake
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    python3
    nethack
    graphviz
    qemu
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
    interactiveShellInit = ''
        any-nix-shell fish --info-right | source
        starship init fish | source
      '';

    plugins = [];
  };

  programs.starship = {
    enable = true;
  };

  services.lorri.enable = true;
  
}
