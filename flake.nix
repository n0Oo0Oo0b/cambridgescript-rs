{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        libPath = with pkgs; lib.makeLibraryPath [
          libGL
          libxkbcommon
          wayland
          xorg.libX11
          xorg.libXcursor
          xorg.libXi
          xorg.libXrandr
        ];
      in
      {
        devShell = with pkgs; mkShell {
          buildInputs = [
            cargo
            cargo-insta
            pre-commit
            rust-analyzer
            rustPackages.clippy
            rustc
            rustfmt
            tokei

            xorg.libxcb
          ];
          RUST_SRC_PATH = rustPlatform.rustLibSrc;
          LD_LIBRARY_PATH = libPath;
        };
      });
}
