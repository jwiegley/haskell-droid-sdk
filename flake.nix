{
  description = "Factory Droid SDK for Haskell development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/5545adfad2e98de106a5544ca7067e03010410bd";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
    in
    {
      devShells = nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          haskell = pkgs.haskell.packages.ghc910;
        in
        {
          default = pkgs.mkShell {
            packages = [
              (haskell.ghcWithPackages (p: [ p.aeson p.tasty p.tasty-hunit p.tasty-quickcheck ]))
              pkgs.cabal-install
              pkgs.ormolu
              pkgs.hlint
              pkgs.python3
              pkgs.pkg-config
              pkgs.zlib
              pkgs.openssl
            ];
          };
        });
    };
}
