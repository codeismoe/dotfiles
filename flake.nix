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

    localbin = {
      url = "path:./bin/";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
  };

  outputs = { home-manager, lix-module, localbin, nixpkgs, ... } @ inputs: rec {
    nixosConfigurations = {
          catbrick = nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            modules = [
              lix-module.nixosModules.default
              ./system.nix 
              home-manager.nixosModules.home-manager
              {
                home-manager.backupFileExtension = "backup1-";
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users =
                  let
                    config = import ./config/default.nix { pkgs = import nixpkgs { inherit system; }; };
                  in {
                  catbrick = import ./users/catbrick.nix {
                    inherit inputs;
                    waybar-config = config.waybar-config;
                  };
                };
              }
            ];
            specialArgs = {
              inherit inputs;
            };
          };
        };
  };
}
