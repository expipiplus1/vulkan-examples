{ pkgs ? import <nixpkgs> {} }:

pkgs.haskell.lib.disableSharedExecutables
  (pkgs.haskellPackages.callCabal2nix "vulkan-examples" ./. {})
