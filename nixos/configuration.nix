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

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

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
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCjDXb8JmdWxW40RS7TMm8xz9kadlo+CAdYrgGps5cIAXBD6+fdLbEjB66vu9YxG/WUUN7lOdRvh/aYJqzh+yLGD06/JSTZXdn8NnCkYBV70bUNzp/GqYaX+t7UdL8eF60p8HOQQVmOYUjbPX0eBEi4b6iDoT+3mO2m5CbLl19iJadTjB0lbJyRNC6BECHhUC+bCXaCLRLTr66f8qShAXup7UAi9v92UXPfm0c+ZaVWoIEjQnRH15VJoFo6geHhmUTSK8pJq+pUCpEiP9ryV/7fBuw2o9OVrP95dxt48cvjOnXq4wGj+DKvG7eCXKM8jC3tJOaUAktgk4Ky/x/fXv+v"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCc9AYDYSRoouPFXUnHGZymOD1X2spB40ceGfg3Pj9UIXagjQKOP3IQHrV0Ems9je8ka+HlYMRrrlS/uwA8HOwENnTx2D0BZTl8eYMERNU05M9QihXDu71C3nhTJsGVvGhD6NxdNmXYB/9NaIP7pCMt8k50HCbY7BDI8HLeP2GULP/6Y/9xtWoXo3D0MYaQo9IRD2RC+CZGIeM3qn8pm+ZDzR9uQOkBp1jCxnEYO4k1XkQyiJ/Rm6BHQ9tQb/dI8zJlNuod3L1D0QN5kVyedz/zd+UOFTcwxL4R1mVIVBLQbQS4pjBFHCa3BFxqsd8bgkmvRzbXP1tlcsBvmf4ioOJD JuiceSSH "
      ];
    };
  };
}
