{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "nixpkgs";
  inputs.feedback.url = github:NorfairKing/feedback;

  outputs = { self, nixpkgs, feedback }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        xp-asm = final.haskellPackages.callCabal2nix "xp-asm" ./. {};
      });
      packages = forAllSystems (system: {
        xp-asm = nixpkgsFor.${system}.xp-asm;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.xp-asm);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [
            self.packages.${system}.xp-asm
          ];
          withHoogle = true;
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
            haskellPackages.cabal-install
            feedback.packages.${system}.default
          ];
        # Change the prompt to show that you are in a devShell
        # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
