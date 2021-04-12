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
      editor = "nvim";
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
      interactiveShellInit = ''
        set -g theme_nerd_fonts yes
      '';
      promptInit = ''
        any-nix-shell fish --info-right | source
      '';

      plugins = [
        {
          name = "theme-bobthefish";
          src = pkgs.fetchFromGitHub {
            owner = "oh-my-fish";
            repo = "theme-bobthefish";
            rev = "626bd39b002535d69e56adba5b58a1060cfb6d7b";
            sha256 = "cd49e0a846601cb9a5cafa2254edccc094d216c603eeddd788feb233398c901b";
          };
        }
      ];
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
            plugin = base16-vim;
            config = "colorscheme base16-gruvbox-dark-pale";
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
          taskwiki
          {
            plugin = vimwiki;
            config = ''
              let g:vimwiki_list = [{'path': '~/Documents/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
              let g:vimwiki_markdown_link_ext = 1
              let g:taskwiki_markup_syntax_ext = 1
              let g:markdown_folding = 1
            '';
          }

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
