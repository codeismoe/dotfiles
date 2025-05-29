{
  description = "Flake for local waybar configuration";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      packageName = "waybar-config";
    in {
      packages.${packageName} = import ./default.nix { inherit pkgs packageName; };
      defaultPackage = self.packages.${system}.${packageName};
    }
  );
}
