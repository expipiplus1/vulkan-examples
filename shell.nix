with (import <nixpkgs> {}).pkgs;

(pkgs.haskellPackages.callPackage ./. {
    vulkan = pkgs.haskellPackages.callPackage /home/j/projects/vulkan/default.nix {};
}).env
