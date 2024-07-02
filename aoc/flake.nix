{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Advent of Code solutions";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
      packageName = "aoc";
    in
    {
      overlay = final: prev: {
        "${packageName}" = final.haskellPackages.callCabal2nix packageName ./. { };
      };
      packages = forAllSystems (system: {
        "${packageName}" = nixpkgsFor.${system}.${packageName};
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.${packageName});
      checks = self.packages;
      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in
        pkgs.haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.${packageName} ];
          withHoogle = true;
          buildInputs = [ pkgs.aoc-cli ] ++ (with pkgs.haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            weeder
          ]);
        });
    };
}
