{ config, pkgs, inputs, ... }:

{
  imports = [
  ];

  nixpkgs.overlays = [
    inputs.niri.overlays.niri
  ];

  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = [ "nix-command flakes" ];
  nix.settings.substituters = [
    "https://cache.nixos-cuda.org"
    "https://nix-community.cachix.org"
    "https://cache.nixos.org/"
  ];

  nix.settings.trusted-public-keys = [
    "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  hardware.bluetooth.enable = true;
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
  
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;

  # security.pki.certificateFiles = [
  #   ./mitmproxy-ca-cert.cer
  #   ./mitmproxy-ca-cert.pem
  # ];

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
  services.power-profiles-daemon.enable = true;
  services.displayManager.gdm = {
    enable = true;
    wayland = true;
  };
  services.desktopManager.gnome.enable = true;
  services.gnome.core-apps.enable = false;
  services.gnome.core-developer-tools.enable = false;
  services.gnome.games.enable = false;
  services.udisks2.enable = true;
  programs.dconf.enable = true;

  xdg.icons.enable = true;
  xdg.mime.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };



  environment.systemPackages = with pkgs; [
    vim
    wget
    networkmanagerapplet
    git
    cachix
    swaylock
    swayidle
    mesa_glu
    ncurses

    linuxPackages.nvidiaPackages.stable
    cudatoolkit
  ];

  services.gnome.gnome-keyring.enable = true;
  services.openssh.enable = true;
  
  programs.nix-ld.enable = true;
  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;
  programs.steam.enable = true;
  programs.hamster.enable = true;
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };

  programs.virt-manager.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      vhostUserPackages = [ pkgs.virtiofsd ];
      runAsRoot = true;
      swtpm.enable = true;
    };
  };

  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/everforest.yaml";
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

  services.emacs.defaultEditor = true;
  
  system.stateVersion = "25.11";
}
