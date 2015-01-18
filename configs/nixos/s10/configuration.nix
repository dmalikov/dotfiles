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
      chromium
      curl
      dhcp
      dmenu
      fuse_exfat
      git
      htop
      i3status
      imagemagick
      iotop
      lsof
      ncmpcpp
      nix-repl
      nmap
      pmutils
      rxvt_unicode
      sshfsFuse
      tig
      tmux
      tree
      unclutter
      vifm
      vim
      wget
      wgetpaste
      zsh
    ];
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
    wireless.enable = true;
  };

  nixpkgs.config = {

    allowUnfree = true;

    chromium = {
      enablePerpperFlash = true;
      enablePepperPDF = true;
    };

  };


  powerManagement.cpuFreqGovernor = "ondemand";

  programs.zsh.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  services.xserver = {
    enable = true;
    synaptics.enable = true;
    windowManager = {
      default = "i3";
      i3.enable = true;
    };
    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
    displayManager = {
      sessionCommands = ''
# enable ctrl-alt-backspace sequence
setxkbmap -option terminate:ctrl_alt_bksp

# enable keyboard layout
setxkbmap -option "" -layout "us,ru" -option grp:caps_toggle

# urxvt fonts
# xset +fp /usr/share/fonts/terminus

# set up delay and rate
xset r rate 300 50

# enable right alt for xcompose
setxkbmap -option compose:ralt

# fix xcompose in gtk applications
export GTK_IM_MODULE=xim

# disable Display Power Managing Signaling
xset -dpms

# process urxvt settings
xrdb ~/.Xresources
xrdb -merge ~/.urxvt/colors/hybrid

# run unclutter to hide cursor
killall unclutter
unclutter &

# run ssh-agent
killall ssh-agent
ssh-agent
ssh-add ~/.ssh/id_rsa
ssh-add ~/.ssh/id_dsa

exec i3 -V >> ~/.i3/logs 2>&1
      '';
    };
  };

  services.logind.extraConfig = "HandleLidSwitch=ignore";

  time.timeZone = "Europe/Prague";

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
