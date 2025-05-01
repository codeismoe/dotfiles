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

    config-files = {
      url = "path:config/";
    };
  };

  outputs = { home-manager, lix-module, nixpkgs, ... } @ inputs: rec {
    nixosConfigurations = {
          catbrick = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              lix-module.nixosModules.default
              ./system.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.backupFileExtension = "backup1-";
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users = {
                  catbrick = (import ./users/catbrick.nix) { inherit inputs; };
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
