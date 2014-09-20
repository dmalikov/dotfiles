{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  environment = with pkgs; {
    systemPackages = [
      curl
      dhcp
      git
      haskellPackages.hdevtools
      haskellPackages.stylishHaskell
      htop
      iotop
      nix-repl
      ncmpcpp
      nmap
      tig
      tmux
      tree
      vifm
      vim
      wget
      wgetpaste
      zsh
    ];
  };

  fileSystems."/mnt/wdt" = {
    device = "/dev/disk/by-label/wdt";
    fsType = "ext4";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = [
      pkgs.terminus_font
    ];
  };

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-i16n.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    firewall.enable = false;
    hostName = "nixos";
    wireless.enable = false;
  };

  nixpkgs.config.allowUnfree = true;

  powerManagement.cpuFreqGovernor = "ondemand";

  programs.zsh.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  services.mpd = {
    enable = true;
    extraConfig = ''
        audio_output {
            type "alsa"
            name "E-MU 0204 USB"
            device "plughw:1,0"
            format "88200:16:2"
        }
    '';
  };

  services.openssh.enable = true;

  services.transmission = {
    enable = true;
  };

  services.logind.extraConfig = "HandleLidSwitch=ignore";

  time.timeZone = "Europe/Moscow";

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";
    extraUsers.yep = {
      name = "yep";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/yep";
      extraGroups = [ "wheel" "audio" ];
      useDefaultShell = true;
    };
  };
}
