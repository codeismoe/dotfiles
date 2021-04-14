{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;

  environment.systemPackages = [ pkgs.neovim pkgs.joplin-desktop pkgs.python39 ];

  fonts = {
    enableFontDir = true;
    fonts = [ pkgs.nerdfonts ];
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  environment.shells = [ pkgs.fish ];
  environment.systemPath = [ /run/current-system/sw/bin ];
  users.users.pks.shell = pkgs.bashInteractive;
  programs.fish.enable = true;

  home-manager.users.pks = { pkgs, ...}: {
    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = with pkgs; [
      darwin.apple_sdk.frameworks.Cocoa
      racket-minimal
      direnv
      elixir
      nodejs
      python39Packages.pip
      python39Packages.pylint
      python39Packages.tasklib
      python39Packages.pynvim
      ghc
      cabal-install
      fish
      starship
      jq
      taskwarrior
      tasksh
      timewarrior
      git
      gnupg
      ripgrep
      rustup
      any-nix-shell
      docker
      docker-compose
    ];

    programs.tmux = {
      enable = true;
      escapeTime = 0;
    };

    programs.git = {
      enable = true;
      userName = "Peter Steidel";
      userEmail = "pks@codeis.moe";
      ignores = [ ".DS_Store" "*~" "*.swp" ];
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
      settings = {
        nix.symbol = " ";
        aws = { disabled=true; symbol = " "; };
        git_branch.smbol = " ";
        docker.symbol = " ";
        python.symbol = " ";
        elixir.symbol = " ";
        package.symbol = " ";
        rust.symbol = " ";
      };
    };

    programs.neovim = {
      enable = true;
      withPython3 = true;
      extraPython3Packages = ps: with ps; [ tasklib pynvim ];
      withNodeJs = true;
      plugins = with pkgs.vimPlugins; [
          vinegar
          vim-polyglot
          vim-racket
          { 
            plugin = nord-vim;
            config = "colorscheme nord";
          }

          tabular
          vim-markdown
          vim-commentary
          vim-airline
          vim-airline-themes
          vim-airline-clock

          coc-nvim
          coc-rls
          coc-css
          coc-git
          coc-python

          fzf-vim

          denite
          vim-snippets
      ];
      extraConfig = (builtins.readFile ./init.vim);
    };
  };

  system.defaults = {
    dock = {
      autohide = true;
      orientation = "bottom";
    };
    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
    };
    NSGlobalDomain = {
      _HIHideMenuBar = true;
      "com.apple.swipescrolldirection" = false;
    };
  };


  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
