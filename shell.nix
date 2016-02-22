with (import <nixpkgs> {}).pkgs;

(pkgs.haskell.packages.ghc801.callPackage ./. {  
    vulkan = pkgs.haskell.packages.ghc801.callPackage /home/jophish/projects/vulkan/default.nix {};
}).env
