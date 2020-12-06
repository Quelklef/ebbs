{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.python38
    pkgs.python38Packages.fire
    pkgs.python38Packages.sympy
  ];
}
