{ haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/fdf3eacf43b2a625ea163f6d45694b523baa1ccf.tar.gz") {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
}:

let
  hsPkgs = pkgs.haskell-nix.project {
    compiler-nix-name = "ghc8104";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "wai-static-th";
      src = ./.;
    };
  };
in
{
  inherit pkgs hsPkgs;
  shell = hsPkgs.shellFor {

    packages = ps: with ps; [
      wai-static-th
    ];
    
    withHoogle = false;
    exactDeps = true;

    tools = {
      cabal-install = "latest";
      haskell-language-server = "latest";
      ghcid = "latest";
    };
  };
}
