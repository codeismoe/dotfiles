{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "pks";
  home.homeDirectory = "/home/pks";

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    firefox
    nyxt
    mpv
    kitty
    any-nix-shell
    emacs

    sbcl
    lispPackages.quicklisp
    roswell

    fira-code
    iosevka 
    elixir
    elixir_ls

    python39
    python-language-server
    
    htop
    nvtop
    ispell

    pavucontrol
    filezilla
    discord
    steam

    emacs-all-the-icons-fonts

    krita
    gimp
    texlive.combined.scheme-basic

    wget

    multimc
    jdk16
    # zulu8

    rofi
    unzip

    octaveFull

    libreoffice-fresh

    ripgrep
    dwarf-fortress-packages.dwarf-fortress-full
    dwarf-therapist

    niv
    stack

    autoconf
    automake

    xmobar
    trayer
    feh

    ledger
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  programs.tmux = {
	  enable = true;
	  escapeTime = 0;
  };

  programs.direnv = {
    enable = true;
  };

  programs.git = {
	  enable = true;
	  userName = "Peter Steidel";
	  userEmail = "psteidelprogramming@gmail.com";
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
  programs.neovim = {
    enable = true;
    withPython3 = true;
    withNodeJs = true;
    plugins = with pkgs.vimPlugins; [
      vinegar
      vim-polyglot
      nord-vim
      tabular
      vim-markdown
      vim-commentary
      vim-airline
      vim-airline-themes
      vim-airline-clock

      coc-nvim
      coc-snippets

      fzf-vim

      denite
      vim-snippets
    ];
    extraConfig = (builtins.readFile ./init.vim);
	};

  services.lorri.enable = true;

  services.taffybar = {
    enable = true;
  };
  
  home.stateVersion = "21.11";
}
