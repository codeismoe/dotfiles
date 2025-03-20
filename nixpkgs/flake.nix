{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/";
    };

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-2.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager.url = "github:nix-community/home-manager";

    base16.url = "github:SenchoPens/base16.nix";

    tinted-terminal = {
      url = "github:tinted-theming/tinted-terminal";
      flake = false;
    };

    tinted-waybar = {
      url = "github:tinted-theming/base16-waybar";
      flake = false;
    };

    tinted-emacs = {
      url = "github:tinted-theming/base16-emacs";
      flake = false;
    };

    base16-sway = {
      url = "github:rkubosz/base16-sway";
      flake = false;
    };

    config-files = {
      url = "path:config/";
    };
  };

  outputs = { home-manager, lix-module, nixpkgs, base16, ... } @ inputs: {
    nixosConfigurations = {
          catbrick = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              lix-module.nixosModules.default
              base16.nixosModule
              { scheme = "${inputs.config-files}/theme.yaml"; }
              ./themeing.nix
              ./system.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.backupFileExtension = "backup-";
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.catbrick = import ./users/catbrick.nix;
              }
            ];
            specialArgs = {
              inherit inputs;
            };
          };
        };
  };
}
