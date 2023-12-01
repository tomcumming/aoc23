{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    hbins.url = "github:tomcumming/haskell-binary-flake";
  };
  outputs = { self, nixpkgs, hbins }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages."${system}";
      hpkgs = hbins.packages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          hpkgs.cabal38
          hpkgs.ghc96
          hpkgs.hls24ghc96
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
