# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, inputs, ... }:

{
  imports = [
  ];

  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    inputs.niri.overlays.niri
    inputs.nix-matlab.overlay
  ]; 
  

  nix = {
    settings.experimental-features = [ "nix-command flakes" ];
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  hardware.bluetooth.enable = true;


  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";

  # Select internationalisation properties.
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

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
  };
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.avahi = {
    publish.enable = true;
    publish.userServices = true;
    enable = true;
    openFirewall = true;
  };

  services.blueman.enable = true;
  
  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.pam.services.swaylock = {};
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
  services.displayManager.gdm.enable = true;
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
    # extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };


  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    networkmanagerapplet
    git
    cachix
    swaylock
    swayidle

    matlab
    mesa_glu
    ncurses

    xorg.libXi
    xorg.libXext
    xorg.libXmu
    xorg.libXp
    xorg.libXpm
    xorg.libXrandr
    xorg.libXrender
    xorg.libXt
    xorg.libXtst
    xorg.libXxf86vm
    xorg.libX11
    zlib
    gdk-pixbuf
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

  # services.samba = {
  #   package = pkgs.samba4Full;
  #   enable = true;
  #   openFirewall = true;
  #   settings = {
  #     global = {
  #       "workgroup" = "WORKGROUP";
  #       "server string" = "SMBNIXSRV";
  #       "netbios name" = "SMBNIXNB";
  #       "security" = "user";
  #       "hosts allow" = "192.168.1. 127.0.0.1 localhost";
  #       "hosts deny" = "0.0.0.0/0";
  #       "guest account" = "nobody";
  #       "map to guest" = "Bad User";
  #     };
  #     "public" = {
  #       "path" = "/mnt/Shares/Public/";
  #       "browseable" = "yes";
  #       "read only" = "yes";
  #       "guest ok" = "yes";
  #     };
  #   };
  # };

  # services.samba-wsdd = {
  #   enable = true;
  #   openFirewall = true;
  # };

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
  
  system.stateVersion = "24.05"; # Did you read the comment?
}
