{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command flakes" ];
  nixpkgs.config = {
    allowUnfree = true;
    nvidia.acceptLicense = true;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "patchwork";
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  
  services.xserver = {
    enable = true;
    videoDrivers = ["nvidia"];
    xkb.layout = "us";
    displayManager.lightdm.enable = true;
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        rofi
        i3status
        i3lock
        xss-lock
        networkmanagerapplet
      ];
    };
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.legacy_470;

    prime = {
      sync.enable = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  services.libinput.enable = true;

  users.users.patchwork = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "video" "power" "games" ];
    shell = pkgs.fish;
  };

  environment.systemPackages = [ pkgs.vim ];

  services.openssh.enable = true;
  
  programs = {
    fish.enable = true;
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
    };
  };

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?
}
