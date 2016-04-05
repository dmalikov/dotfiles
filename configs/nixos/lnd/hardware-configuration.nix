{ config, lib, pkgs, ... }:

{
  imports = 
    [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ];

  boot.initrd.availableKernelModules = [ "virtio_net" "virtio_pci" "virtio_blk" "virtio_scsi" "9p" "9pnet_virtio" "ata_piix" "virtio_pci" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/5ad07845-582a-4787-9437-2256557dbf89";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/01a3be21-9a93-44b9-9944-e14220f1b583";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/e6fd869d-cb74-48b5-947e-ee151f737513"; }
    ];

  nix.maxJobs = 2;
}
