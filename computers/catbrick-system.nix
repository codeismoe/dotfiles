{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
      ./base-system.nix
    ];

  nixpkgs.config.cudaSupport = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [  "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/7adf5ddd-c369-4f47-b30e-027c44d2940e";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/BDD4-7930";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/36778c9c-0a04-4e97-8b60-a4a3bbf26429";
      fsType = "ext4";
    };
  swapDevices = [ ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.xserver.videoDrivers = ["modesetting" "nvidia" ];
  powerManagement.enable = true;
  services.graphical-desktop.enable = lib.mkDefault true;
  hardware.graphics.extraPackages = with pkgs; [
    intel-media-driver
  ];

  hardware.nvidia = {
    open = true;
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  networking.hostName = "catbrick";

  users.groups = {
    catbrick = {};
  };

  users.users = {
    catbrick = {
      isNormalUser = true;
      group = "catbrick";
      description = "Lily Steidel";
      extraGroups = [
        "networkmanager"
        "docker"
        "wheel"
        "input"
        "audio"
        "video"
        "power"
        "games"
        "libvirtd"
      ];
      shell = pkgs.fish;
    };
  };

  # security.pki.certificateFiles = [
  #   ./mitmproxy-ca-cert.pem
  #   ./mitmproxy-ca-cert.cer
  # ];
}
