{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let src = with pkgs.lib;
          let p = n: (toString ./dist) == n;
          in cleanSourceWith {filter = (n: t: !p n); src = cleanSource ./.;};

    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        vector-sized = self.vector-sized_1_0_1_0;
        vulkan = import ../vulkan {inherit pkgs compiler;};
      };
    };

    drv = pkgs.haskell.lib.disableSharedExecutables
      (haskellPackages.callCabal2nix "vulkan-examples" ./. {});
in
  drv
