# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, inputs, ... }:

{
  imports = [
    ./patchwork-hardware.nix
  ];

  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    inputs.niri.overlays.niri
  ];

  nixpkgs.config = {
    allowUnfree = true;
    nvidia.acceptLicense = true;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix = {
    settings.experimental-features = [ "nix-command flakes" ];
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.bluetooth.enable = true;

  networking.hostName = "patchwork";
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.xserver = {
    enable = true;
  };

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.blueman.enable = true;
  
  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  fonts.fontDir.enable = true;
  fonts.packages = (with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.fira-code
    source-sans
    noto-fonts
    noto-fonts-color-emoji
    iosevka
    fira-code
    fira-code-symbols
    _3270font
    ibm-plex
  ]);

  services.dbus.enable = true;

  services.displayManager.gdm.enable = true;
  programs.dconf.enable = true;

  xdg.icons.enable = true;
  xdg.mime.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  users.groups = {
    patchwork = {};
  };

  users.users = {
    patchwork = {
      isNormalUser = true;
      group = "patchwork";
      description = "PatchWork";
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

  environment.systemPackages = with pkgs; [
    vim
    wget
    networkmanagerapplet
    git
  ];

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;
  services.openssh.enable = true;

  programs = {
    nix-ld.enable = true;
    fish.enable = true;
    gnupg.agent = {
      enable = true;
    };
    hamster.enable = true;
    niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
  };

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;

  networking.firewall.checkReversePath = false;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/horizon-dark.yaml";
    image = ../bg-3.jpg;
    polarity = "dark";
    
    fonts.monospace.package = pkgs.nerd-fonts.iosevka;
    fonts.monospace.name = "Iosevka Nerd Font";

    fonts.sansSerif.package = pkgs.nerd-fonts.ubuntu;
    fonts.sansSerif.name = "Ubuntu Nerd Font";

    fonts.serif = config.stylix.fonts.sansSerif;

    fonts.sizes.applications = 10;
    fonts.sizes.desktop = 12;

    cursor.package = pkgs.phinger-cursors;
    cursor.name = "phinger-cursors-dark";
    cursor.size = 12;
  };

  system.stateVersion = "25.11";
}
