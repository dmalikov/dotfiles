# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.kernelParams = [ "console=ttyS0" ];
  boot.loader.grub.extraConfig = "serial; terminal_input serial; terminal_output serial";
  boot.loader.grub.device = "/dev/sda";

  environment = with pkgs; {
    systemPackages = [
      git gnumake htop iotop lsof nix-repl python tig tmux tree vim wget zsh
    ];
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Amsterdam";

  services.openssh.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  boot.initrd.checkJournalingFS = false;

  programs.zsh.enable = true;

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";
    extraUsers.m = {
      name = "m";
      group = "users";
      extraGroups = [ "wheel" ];
      uid = 1000;
      createHome = true;
      home = "/home/m";
      useDefaultShell = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCjDXb8JmdWxW40RS7TMm8xz9kadlo+CAdYrgGps5cIAXBD6+fdLbEjB66vu9YxG/WUUN7lOdRvh/aYJqzh+yLGD06/JSTZXdn8NnCkYBV70bUNzp/GqYaX+t7UdL8eF60p8HOQQVmOYUjbPX0eBEi4b6iDoT+3mO2m5CbLl19iJadTjB0lbJyRNC6BECHhUC+bCXaCLRLTr66f8qShAXup7UAi9v92UXPfm0c+ZaVWoIEjQnRH15VJoFo6geHhmUTSK8pJq+pUCpEiP9ryV/7fBuw2o9OVrP95dxt48cvjOnXq4wGj+DKvG7eCXKM8jC3tJOaUAktgk4Ky/x/fXv+v"
      ];
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;
    httpConfig = ''
      sendfile on;
      tcp_nopush on;
      tcp_nodelay on;
      keepalive_timeout 65;
      types_hash_max_size 2048;

      gzip on;
      gzip_disable "msie6";

      server {
        listen 80;
        listen [::]:80 ipv6only=on;

        root /srv/whatever;
        index index.html index.htm;

        location / {
          try_files $uri $uri/ /index.html;
        }
      }
    '';
  };
}
