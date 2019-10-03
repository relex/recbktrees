{ pkgs ? import <nixpkgs> {} }:

rec {
  recbktree = pkgs.haskellPackages.callPackage ./default.nix {};
  shell = pkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with pkgs.haskellPackages; [
      hlint
      ghcid
      cabal-install
      (ghcWithPackages (_: recbktree.buildInputs ++ recbktree.propagatedBuildInputs))
    ];
  };
}
