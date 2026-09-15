{
  description = "Factory Droid SDK for Haskell development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/5545adfad2e98de106a5544ca7067e03010410bd";

  outputs = { self, nixpkgs, ... }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
    in
    {
      packages = nixpkgs.lib.genAttrs systems (system:
        let pkgs = import nixpkgs { inherit system; }; in {
          factory-droid-validator = pkgs.rustPlatform.buildRustPackage {
            pname = "factory-droid-validator";
            version = "0.1.0";
            src = ./native/schema-validator;
            cargoLock.lockFile = ./native/schema-validator/Cargo.lock;
            postInstall = ''
              install -Dm644 THIRD_PARTY_LICENSES.txt $out/share/doc/factory-droid-validator/THIRD_PARTY_LICENSES.txt
            '';
            meta.license = pkgs.lib.licenses.asl20;
            meta.mainProgram = "factory-droid-validator";
          };
        });
      devShells = nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          ghc = pkgs.haskell.packages.ghc9124.ghc;
          linuxSystem = if builtins.elem system [ "aarch64-darwin" "aarch64-linux" ] then "aarch64-linux" else "x86_64-linux";
          linux = import nixpkgs { system = linuxSystem; };
          linuxToolsFor = compiler: pkgs.linkFarm "droid-sdk-linux-tools" [
            { name = "ghc"; path = compiler; }
            { name = "cabal"; path = linux.cabal-install; }
            { name = "cc"; path = linux.stdenv.cc; }
            { name = "bash"; path = linux.bash; }
            { name = "coreutils"; path = linux.coreutils; }
            { name = "findutils"; path = linux.findutils; }
            { name = "make"; path = linux.gnumake; }
            { name = "pkg-config"; path = linux.pkg-config; }
            { name = "python"; path = linux.python3; }
            { name = "openssl"; path = pkgs.lib.getBin linux.openssl; }
            { name = "zlib"; path = pkgs.lib.getLib linux.zlib; }
            { name = "zlib-dev"; path = pkgs.lib.getDev linux.zlib; }
            { name = "cargo"; path = linux.cargo; }
            { name = "rustc"; path = linux.rustc; }
            { name = "cargo-vendor"; path = self.packages.${system}.factory-droid-validator.cargoDeps; }
          ];
          shellFor = compiler: pkgs.mkShell {
            packages = [
              compiler
              pkgs.cabal-install
              pkgs.cargo
              pkgs.rustc
              self.packages.${system}.factory-droid-validator
              pkgs.ormolu
              pkgs.hlint
              pkgs.python3
              pkgs.pkg-config
              pkgs.zlib
              pkgs.openssl
            ];
          };
        in
        {
          default = shellFor ghc;
          ghc912 = shellFor ghc;
          linuxChecks = (shellFor ghc).overrideAttrs (_: {
            DROID_SDK_LINUX_TOOLS = linuxToolsFor linux.haskell.packages.ghc9124.ghc;
          });
          linuxChecks912 = (shellFor ghc).overrideAttrs (_: {
            DROID_SDK_LINUX_TOOLS = linuxToolsFor linux.haskell.packages.ghc9124.ghc;
          });
        });
    };
}
